# API MADRID DATOS REALES
# Función para obtener datos de contaminación tiempo real de Madrid
# URL: https://ciudadesabiertas.madrid.es/dynamicAPI/API/query/calair_tiemporeal.xml

library(httr2)
library(xml2)
library(dplyr)
library(lubridate)
library(logger)
library(sf)

# Cargar tabla de estaciones y magnitudes si existe
if(file.exists("R/utils.R")) {
  source("R/utils.R")
}

#' Obtiene datos REALES de contaminación de Madrid API
#' @param max_registros Máximo número de registros a procesar (para limitar tiempo)
#' @return data.frame con datos de contaminación procesados
obtener_datos_madrid_reales <- function(max_registros = 200) {

  log_info("🌐 Conectando a API REAL Madrid datos tiempo real...")

  url_madrid <- "https://ciudadesabiertas.madrid.es/dynamicAPI/API/query/calair_tiemporeal.xml?pageSize=5000"

  tryCatch({
    # Realizar petición HTTP
    resp <- request(url_madrid) %>%
      req_timeout(20) %>%
      req_perform()

    if(resp_status(resp) != 200) {
      log_warn("API Madrid respondió con status: {resp_status(resp)}")
      return(NULL)
    }

    # Parsear XML
    xml_content <- resp_body_string(resp)
    xml_doc <- read_xml(xml_content)

    # Extraer registros
    registros <- xml_find_all(xml_doc, ".//record")
    log_info("📊 Registros encontrados en API: {length(registros)}")

    if(length(registros) == 0) {
      log_warn("No se encontraron registros en API Madrid")
      return(NULL)
    }

    # Procesar registros (limitar para rendimiento)
    registros_procesar <- registros[1:min(max_registros, length(registros))]
    log_info("🔄 Procesando {length(registros_procesar)} registros...")

    # Función para extraer datos de un registro XML
    extraer_registro <- function(record) {
      provincia <- xml_text(xml_find_first(record, "PROVINCIA"))
      municipio <- xml_text(xml_find_first(record, "MUNICIPIO"))
      estacion <- xml_text(xml_find_first(record, "ESTACION"))
      magnitud <- xml_text(xml_find_first(record, "MAGNITUD"))
      ano <- as.numeric(xml_text(xml_find_first(record, "ANO")))
      mes <- as.numeric(xml_text(xml_find_first(record, "MES")))
      dia <- as.numeric(xml_text(xml_find_first(record, "DIA")))

      # Extraer valores horarios H01-H24
      datos_horas <- list()
      for(h in 1:24) {
        h_campo <- sprintf("H%02d", h)
        v_campo <- sprintf("V%02d", h)

        valor_texto <- xml_text(xml_find_first(record, h_campo))
        validez <- xml_text(xml_find_first(record, v_campo))

        # Convertir valor a numérico si no está vacío
        valor <- ifelse(valor_texto == "" || is.na(valor_texto), NA, as.numeric(valor_texto))

        if(!is.na(valor) && valor > 0) {
          datos_horas[[length(datos_horas) + 1]] <- data.frame(
            provincia = provincia,
            municipio = municipio,
            id_estacion = as.numeric(estacion),
            id_magnitud = as.numeric(magnitud),
            ano = ano,
            mes = mes,
            dia = dia,
            hora = h - 1, # H01 = hora 0 (00:00-01:00)
            valor = valor,
            validez = validez,
            stringsAsFactors = FALSE
          )
        }
      }

      if(length(datos_horas) > 0) {
        return(do.call(rbind, datos_horas))
      } else {
        return(NULL)
      }
    }

    # Procesar todos los registros
    datos_list <- lapply(registros_procesar, extraer_registro)
    datos_list <- datos_list[!sapply(datos_list, is.null)]

    if(length(datos_list) == 0) {
      log_warn("No se pudieron extraer datos válidos")
      return(NULL)
    }

    # Combinar todos los datos
    datos_madrid <- do.call(rbind, datos_list)

    # Crear fecha y hora completa
    datos_madrid <- datos_madrid %>%
      mutate(
        fecha = as.Date(paste(ano, mes, dia, sep = "-")),
        fecha_hora = as.POSIXct(paste(fecha, sprintf("%02d:00:00", hora)),
                               format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Madrid"),
        fuente = "api_madrid_real",
        timestamp_obtencion = Sys.time()
      )

    # Filtrar solo datos del día actual (tiempo real)
    fecha_hoy <- Sys.Date()
    datos_madrid <- datos_madrid %>%
      filter(fecha >= fecha_hoy - 1) # Incluir ayer para datos de madrugada

    log_info("✅ Datos Madrid REALES procesados: {nrow(datos_madrid)} registros")

    # Mapear magnitudes a nombres de contaminantes
    magnitudes_madrid <- data.frame(
      id_magnitud = c(1, 6, 7, 8, 9, 10, 12, 14, 20, 30, 35),
      contaminante = c(
        "Dióxido de Azufre", "Monóxido de Carbono", "Monóxido de Nitrógeno",
        "Dióxido de Nitrógeno", "Partículas < 2.5 µm", "Partículas < 10 µm",
        "Óxidos de Nitrógeno", "Ozono", "Tolueno", "Benceno", "Monóxido de Carbono"
      ),
      unidad = c(
        "µg/m³", "mg/m³", "µg/m³", "µg/m³", "µg/m³", "µg/m³",
        "µg/m³", "µg/m³", "µg/m³", "µg/m³", "mg/m³"
      ),
      stringsAsFactors = FALSE
    )

    # Unir con nombres de contaminantes
    datos_madrid <- datos_madrid %>%
      left_join(magnitudes_madrid, by = "id_magnitud") %>%
      filter(!is.na(contaminante)) # Solo contaminantes conocidos

    # Agregar coordenadas de estaciones (usando datos conocidos)
    estaciones_coords <- data.frame(
      id_estacion = c(4, 8, 11, 16, 17, 18, 27, 35, 36, 38, 39, 40, 47, 48, 49, 50, 54, 55, 56, 57),
      nombre_estacion = c(
        "Pza. de España", "Escuelas Aguirre", "Av. Ramón y Cajal", "Arturo Soria",
        "Villaverde Alto", "Farolillo", "Barajas Pueblo", "Pza. del Carmen",
        "Moratalaz", "Cuatro Caminos", "Barrio del Pilar", "Vallecas",
        "Mendez Alvaro", "Castellana", "Retiro", "Pza. Castilla",
        "Ensanche Vallecas", "Urb. Embajada", "Pza. Fernandez Ladreda", "Sanchinarro"
      ),
      latitud = c(40.4238, 40.4213, 40.4514, 40.4405, 40.3479, 40.3748, 40.4756, 40.4192,
                  40.4077, 40.4459, 40.4773, 40.3943, 40.3980, 40.4407, 40.4152, 40.4656,
                  40.3735, 40.4531, 40.4019, 40.4848),
      longitud = c(-3.7122, -3.6958, -3.6774, -3.6394, -3.7215, -3.7336, -3.5935, -3.7026,
                   -3.6453, -3.7097, -3.7137, -3.6458, -3.6862, -3.6889, -3.6823, -3.6951,
                   -3.6056, -3.6280, -3.7158, -3.6500),
      tipo_estacion = c("Suburbana", "Industrial", "Urbana", "Urbana", "Urbana", "Urbana",
                        "Suburbana", "Urbana", "Urbana", "Urbana", "Suburbana", "Urbana",
                        "Urbana", "Urbana", "Urbana", "Urbana", "Suburbana", "Suburbana",
                        "Urbana", "Suburbana"),
      stringsAsFactors = FALSE
    )

    # Unir con coordenadas
    datos_madrid <- datos_madrid %>%
      left_join(estaciones_coords, by = "id_estacion") %>%
      filter(!is.na(latitud)) # Solo estaciones con coordenadas conocidas

    # Crear objeto espacial
    if(nrow(datos_madrid) > 0) {
      datos_madrid_sf <- st_as_sf(datos_madrid,
                                  coords = c("longitud", "latitud"),
                                  crs = 4326)

      # Renombrar columnas para consistencia
      datos_madrid_sf <- datos_madrid_sf %>%
        select(
          id_estacion,
          contaminante,
          nombre_estacion,
          tipo_estacion,
          fecha,
          fecha_hora,
          valor_medido = valor,
          valor_medio = valor,
          unidad,
          fuente,
          timestamp_obtencion,
          geometry
        )

      log_success("✅ API Madrid REAL: {nrow(datos_madrid_sf)} registros procesados")
      log_info("🏭 Contaminantes: {paste(unique(datos_madrid_sf$contaminante), collapse = ', ')}")
      log_info("🏢 Estaciones: {length(unique(datos_madrid_sf$id_estacion))}")

      return(datos_madrid_sf)

    } else {
      log_warn("No hay datos válidos después del procesamiento")
      return(NULL)
    }

  }, error = function(e) {
    log_error("Error obteniendo datos API Madrid: {e$message}")
    return(NULL)
  })
}

# Test de la función
if(FALSE) { # Solo ejecutar manualmente
  datos_test <- obtener_datos_madrid_reales()
  if(!is.null(datos_test)) {
    print(head(datos_test))
    cat("Registros obtenidos:", nrow(datos_test), "\n")
  }
}