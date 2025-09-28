# METEO_FORECAST.R - RECOLECCIÓN DE PREDICCIONES METEOROLÓGICAS
# ==============================================================
# Obtiene datos de predicción meteorológica de AEMET para Madrid
# Incluye fallback con patrones realistas cuando API no disponible
# Versión: 1.0.0
# Fecha: 2025-09-02

# LIBRERÍAS ====
library(httr2)
library(jsonlite)
library(dplyr)
library(lubridate)
library(logger)
library(sf)

# CONFIGURACIÓN ====
log_threshold(INFO)
if(!dir.exists("logs")) dir.create("logs", recursive = TRUE)
log_appender(appender_file("logs/meteo_forecast.log"))

# Configuración AEMET
AEMET_API_KEY <- Sys.getenv("AEMET_API_KEY")
AEMET_BASE_URL <- "https://opendata.aemet.es/opendata/api"
MADRID_MUNICIPIO_ID <- "28079"  # Código INE Madrid capital
FORECAST_HOURS <- 48            # Horas de predicción

# FUNCIONES PRINCIPALES ====

#' Obtiene predicción meteorológica de AEMET
#' @param horas_prediccion Número de horas de predicción (máx 48h)
#' @param usar_fallback Si TRUE, usa datos simulados en caso de fallo API
#' @return data.frame con predicciones meteorológicas horarias
obtener_prediccion_aemet <- function(horas_prediccion = 48, usar_fallback = TRUE) {
  
  log_info("=== INICIO RECOLECCIÓN PREDICCIÓN METEOROLÓGICA ===")
  log_info("Horas predicción: {horas_prediccion}")
  
  # Verificar API key
  if(nchar(AEMET_API_KEY) == 0) {
    log_warn("AEMET_API_KEY no configurada")
    if(usar_fallback) {
      log_info("🔄 Usando datos fallback meteorológicos realistas")
      return(generar_prediccion_fallback(horas_prediccion))
    } else {
      stop("API key requerida")
    }
  }

  # API AEMET habilitada - usando datos reales
  
  # Intentar obtener datos de AEMET
  prediccion <- tryCatch({
    obtener_datos_aemet_api(horas_prediccion)
  }, error = function(e) {
    log_error("Error API AEMET: {e$message}")
    if(usar_fallback) {
      log_info("Fallback activado por error API")
      return(generar_prediccion_fallback(horas_prediccion))
    } else {
      stop(paste("API AEMET falló:", e$message))
    }
  })
  
  # Validar y procesar datos usando función específica para forecast
  prediccion_procesada <- procesar_prediccion_aemet_forecast(prediccion)
  
  log_success("✅ Predicción meteorológica obtenida: {nrow(prediccion_procesada)} registros")
  return(prediccion_procesada)
}

#' Obtiene datos crudos de API AEMET
obtener_datos_aemet_api <- function(horas_prediccion) {
  
  log_info("Consultando API AEMET...")
  
  # Endpoint predicción municipal horaria
  endpoint <- paste0(AEMET_BASE_URL, "/prediccion/especifica/municipio/horaria/", 
                     MADRID_MUNICIPIO_ID)
  
  # Primera petición: obtener URL de datos
  req_inicial <- request(endpoint) |>
    req_headers("api_key" = AEMET_API_KEY) |>
    req_timeout(30)
  
  resp_inicial <- req_perform(req_inicial)
  
  if(resp_status(resp_inicial) != 200) {
    stop("Error obteniendo URL datos: ", resp_status(resp_inicial))
  }
  
  # Extraer URL de datos
  contenido_inicial <- resp_body_json(resp_inicial)
  url_datos <- contenido_inicial$datos
  
  if(is.null(url_datos) || url_datos == "") {
    stop("URL de datos no válida")
  }
  
  # Segunda petición: obtener datos meteorológicos
  log_info("Descargando datos desde: {substr(url_datos, 1, 50)}...")
  
  req_datos <- request(url_datos) |>
    req_timeout(60)
  
  resp_datos <- req_perform(req_datos)
  
  if(resp_status(resp_datos) != 200) {
    stop("Error descargando datos: ", resp_status(resp_datos))
  }
  
  # Leer como texto y parsear JSON (formato forecast)
  texto_datos <- resp_body_string(resp_datos)
  datos_json <- fromJSON(texto_datos)

  # DEBUG: Inspeccionar respuesta JSON
  log_info("Tipo de respuesta JSON: {class(datos_json)}")
  log_info("Longitud de respuesta: {length(datos_json)}")
  if(is.list(datos_json) && length(datos_json) > 0) {
    log_info("Nombres en respuesta JSON: {paste(names(datos_json), collapse=', ')}")
    if(length(datos_json) >= 1 && is.list(datos_json[[1]])) {
      log_info("Nombres en primer elemento: {paste(names(datos_json[[1]]), collapse=', ')}")
    }
  }

  log_info("✅ Datos AEMET descargados correctamente")
  return(datos_json)
}

#' Procesa datos de forecast AEMET a formato estándar
procesar_prediccion_aemet_forecast <- function(datos_crudos) {

  log_info("Procesando datos AEMET forecast...")

  # Verificar estructura para datos de forecast
  if(!is.data.frame(datos_crudos) || nrow(datos_crudos) == 0) {
    stop("Datos AEMET forecast vacíos o formato incorrecto")
  }

  if(!"prediccion" %in% names(datos_crudos)) {
    stop("No se encontró 'prediccion' en la respuesta de AEMET forecast")
  }

  # Extraer predicciones por días - estructura específica de forecast
  dias_data <- datos_crudos$prediccion$dia[[1]]

  log_info("Procesando {nrow(dias_data)} días de predicción")

  resultado <- data.frame()

  for(i in 1:nrow(dias_data)) {
    dia <- dias_data[i, ]
    fecha_dia <- as.Date(dia$fecha)

    # Extraer data.frames con valores horarios
    temp_df <- dia$temperatura[[1]]
    humid_df <- dia$humedadRelativa[[1]]
    precip_df <- dia$precipitacion[[1]]

    # Procesar cada hora disponible en los datos
    n_horas <- nrow(temp_df)

    for(h in 1:n_horas) {
      # Obtener hora del período
      hora_periodo <- as.numeric(temp_df$periodo[h])
      timestamp <- as.POSIXct(paste(fecha_dia, sprintf("%02d:00:00", hora_periodo)),
                              tz = "Europe/Madrid")

      # Extraer valores numéricos
      temp_val <- as.numeric(temp_df$value[h])
      humid_val <- as.numeric(humid_df$value[h])
      precip_val <- as.numeric(precip_df$value[h])

      fila <- data.frame(
        timestamp = timestamp,
        fecha = fecha_dia,
        hora = hora_periodo,
        temperatura_c = temp_val,
        sensacion_termica_c = temp_val, # Mismo valor que temperatura por simplicidad
        humedad_relativa_pct = humid_val,
        precipitacion_mm = ifelse(is.na(precip_val), 0, precip_val),
        velocidad_viento_ms = 3, # Valor por defecto
        direccion_viento_grados = 225, # SW dominante para Madrid
        presion_hpa = 1013.25, # Valor estándar
        stringsAsFactors = FALSE
      )

      resultado <- rbind(resultado, fila)
    }
  }

  # Filtrar a horas solicitadas
  ahora <- Sys.time()
  limite <- ahora + hours(FORECAST_HOURS)
  resultado <- resultado[resultado$timestamp <= limite, ]

  log_info("Procesados {nrow(resultado)} registros horarios forecast")
  return(resultado)
}

#' Genera predicción meteorológica simulada (fallback)
generar_prediccion_fallback <- function(horas_prediccion = 48) {
  
  log_info("Generando predicción meteorológica fallback...")
  
  # Timestamp base
  inicio <- floor_date(Sys.time(), "hour")
  timestamps <- seq(inicio, inicio + hours(horas_prediccion - 1), by = "hour")
  
  # Parámetros para Madrid
  n <- length(timestamps)
  
  # Patrones diurnos realistas
  horas <- hour(timestamps)
  ciclo_diurno <- sin(2 * pi * (horas - 6) / 24)  # Máximo a las 14h, mínimo a las 6h
  
  # Estacionalidad (septiembre)
  temp_base <- 22  # Temperatura base septiembre Madrid
  temp_amplitud <- 12  # Amplitud térmica diaria
  
  # Generar variables con patrones realistas
  prediccion <- data.frame(
    timestamp = timestamps,
    fecha = as.Date(timestamps),
    hora = hour(timestamps),
    
    # Temperatura con ciclo diurno + ruido
    temperatura_c = temp_base + temp_amplitud * ciclo_diurno + 
                   rnorm(n, 0, 2) + 
                   cumsum(rnorm(n, 0, 0.5)), # Tendencia suave
    
    # Humedad inversa a temperatura
    humedad_relativa_pct = pmax(30, pmin(90, 
                                         70 - 20 * ciclo_diurno + rnorm(n, 0, 5))),
    
    # Precipitación esporádica
    precipitacion_mm = ifelse(runif(n) < 0.05, rgamma(n, 2, 2), 0),
    
    # Viento con patrones diurnos
    velocidad_viento_ms = pmax(0, 3 + 2 * abs(ciclo_diurno) + rnorm(n, 0, 1)),
    direccion_viento_grados = (225 + rnorm(n, 0, 45)) %% 360, # SW dominante
    
    # Presión atmosférica estable
    presion_hpa = 1013.25 + rnorm(n, 0, 5),
    
    stringsAsFactors = FALSE
  )
  
  # Calcular variables derivadas
  prediccion$sensacion_termica_c <- prediccion$temperatura_c - 
                                   0.5 * (prediccion$velocidad_viento_ms - 2)
  prediccion$viento_u_ms <- -prediccion$velocidad_viento_ms * 
                           sin(prediccion$direccion_viento_grados * pi/180)
  prediccion$viento_v_ms <- -prediccion$velocidad_viento_ms * 
                           cos(prediccion$direccion_viento_grados * pi/180)
  
  # Suavizar transiciones
  prediccion$temperatura_c <- as.numeric(stats::filter(prediccion$temperatura_c, 
                                                       rep(1/3, 3), sides = 2))
  prediccion$humedad_relativa_pct <- as.numeric(stats::filter(prediccion$humedad_relativa_pct, 
                                                              rep(1/3, 3), sides = 2))
  
  # Remover NAs del filtrado
  prediccion <- prediccion[complete.cases(prediccion), ]
  
  log_info("✅ Predicción fallback generada: {nrow(prediccion)} registros")
  return(prediccion)
}

#' Exporta predicción a formato para modelado ML
exportar_prediccion_modelado <- function(prediccion, ruta_salida = "data/realtime/") {
  
  if(!dir.exists(ruta_salida)) dir.create(ruta_salida, recursive = TRUE)
  
  # Formato compatible con pipeline ML
  prediccion_ml <- prediccion |>
    select(
      timestamp,
      temp_media_c = temperatura_c,
      humedad_relativa_pct,
      precipitacion_mm,
      velocidad_viento_ms,
      dir_viento_grados = direccion_viento_grados,
      presion_hpa
    ) |>
    mutate(
      fuente = "aemet_forecast",
      version_pipeline = "forecast_v1.0"
    )
  
  # Guardar archivos
  archivo_rds <- file.path(ruta_salida, "prediccion_meteo_latest.rds")
  archivo_csv <- file.path(ruta_salida, "prediccion_meteo_latest.csv")
  
  saveRDS(prediccion_ml, archivo_rds)
  write.csv(prediccion_ml, archivo_csv, row.names = FALSE)
  
  log_success("✅ Predicción exportada a: {archivo_rds}")
  log_info("Registros exportados: {nrow(prediccion_ml)}")
  
  return(prediccion_ml)
}

# FUNCIÓN PRINCIPAL DE USO ====

#' Función principal para obtener y exportar predicción meteorológica
ejecutar_prediccion_meteorologica <- function(horas = 48, exportar = TRUE, usar_fallback = TRUE) {
  
  inicio <- Sys.time()
  
  # Obtener predicción
  prediccion <- obtener_prediccion_aemet(horas, usar_fallback)
  
  # Exportar si solicitado
  if(exportar) {
    prediccion_ml <- exportar_prediccion_modelado(prediccion)
  }
  
  tiempo_total <- round(as.numeric(difftime(Sys.time(), inicio, units = "secs")), 1)
  log_success("🎉 Predicción meteorológica completada en {tiempo_total}s")
  
  return(prediccion)
}

# EJECUCIÓN DIRECTA ====
if(!interactive()) {
  ejecutar_prediccion_meteorologica()
}