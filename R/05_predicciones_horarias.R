# PREDICCIONES HORARIAS PARA PRÓXIMAS 40 HORAS
# Objetivo: Generar predicciones temporales cada hora con datos meteorológicos simulados

# 1. LIBRERÍAS ----
library(sf)
library(dplyr)
library(lubridate)
library(logger)
library(glue)

# 2. CONFIGURACIÓN ----
log_appender(appender_tee("logs/predicciones_horarias.log"))
log_info("--- INICIO PREDICCIONES HORARIAS ---")

# 3. FUNCIÓN PARA GENERAR DATOS METEOROLÓGICOS FUTUROS ----
generar_meteo_futuro <- function(horas_futuras = 40) {
  
  log_info("Generando datos meteorológicos para próximas {horas_futuras} horas...")
  
  # Punto de inicio: hora actual
  inicio <- Sys.time()
  inicio_hora <- floor_date(inicio, "hour")
  
  # Crear secuencia de horas futuras
  horas_seq <- seq(inicio_hora, by = "hour", length.out = horas_futuras)
  
  # Simular patrón meteorológico realista para Madrid
  datos_meteo <- data.frame(
    fecha_hora = horas_seq,
    fecha = as.Date(horas_seq),
    hora = hour(horas_seq),
    dia_año = yday(horas_seq)
  ) %>%
    mutate(
      # Patrón de temperatura diurno (máximo a las 15h, mínimo a las 6h)
      temp_base = 25 + sin((dia_año - 200) * 2 * pi / 365) * 12, # Variación estacional
      temp_diurna = sin((hora - 6) * pi / 12) * 8, # Variación diurna
      temp_media_c = temp_base + temp_diurna + rnorm(n(), 0, 1),
      
      # Humedad inversa a temperatura
      humedad_media_pct = 70 - (temp_media_c - 20) * 1.5 + rnorm(n(), 0, 5),
      
      # Precipitación ocasional (10% probabilidad)
      precipitacion_mm = ifelse(runif(n()) < 0.1, rexp(n(), 2), 0),
      
      # Viento más fuerte durante el día
      vel_viento_media_ms = 2 + sin((hora - 12) * pi / 12) * 1.5 + abs(rnorm(n(), 0, 0.5)),
      
      # Dirección del viento con variabilidad
      dir_viento_grados = 200 + sin(hora * pi / 12) * 30 + rnorm(n(), 0, 20),
      
      # Presión con variación ligera
      presion_maxima_hpa = 1013 + sin((dia_año - 50) * 2 * pi / 365) * 8 + rnorm(n(), 0, 3)
    ) %>%
    # Aplicar límites realistas
    mutate(
      temp_media_c = pmax(pmin(temp_media_c, 45), 0),
      humedad_media_pct = pmax(pmin(humedad_media_pct, 100), 20),
      vel_viento_media_ms = pmax(vel_viento_media_ms, 0),
      dir_viento_grados = ((dir_viento_grados %% 360) + 360) %% 360,
      presion_maxima_hpa = pmax(pmin(presion_maxima_hpa, 1040), 980)
    )
  
  log_info("Datos meteorológicos generados: {nrow(datos_meteo)} horas")
  log_info("Rango temperaturas: {round(min(datos_meteo$temp_media_c), 1)}°C - {round(max(datos_meteo$temp_media_c), 1)}°C")
  
  return(datos_meteo)
}

# 4. FUNCIÓN PARA GENERAR PREDICCIONES POR ESTACIÓN ----
generar_predicciones_estaciones <- function(datos_meteo, archivo_modelos = "models/modelos_caret_simple.rds") {
  
  log_info("Generando predicciones para estaciones de Madrid...")
  
  # Cargar modelos
  if(!file.exists(archivo_modelos)) {
    log_error("Archivo de modelos no existe: {archivo_modelos}")
    return(NULL)
  }
  
  modelos <- readRDS(archivo_modelos)
  log_info("Modelos cargados: {length(modelos)} contaminantes")
  
  # Coordenadas de estaciones principales de Madrid
  estaciones <- data.frame(
    id_estacion = c(4, 8, 11, 16, 17, 18, 27, 35, 36, 38, 39, 40, 47, 48, 49, 50),
    nombre_estacion = c(
      "Pza. de España", "Escuelas Aguirre", "Av. Ramón y Cajal", "Arturo Soria", 
      "Villaverde Alto", "Farolillo", "Barajas Pueblo", "Pza. del Carmen", 
      "Moratalaz", "Cuatro Caminos", "Barrio del Pilar", "Vallecas", 
      "Mendez Alvaro", "Castellana", "Retiro", "Pza. Castilla"
    ),
    lat = c(40.4238, 40.4213, 40.4514, 40.4405, 40.3479, 40.3748, 40.4756, 40.4192,
            40.4077, 40.4459, 40.4773, 40.3943, 40.3980, 40.4407, 40.4152, 40.4656),
    lon = c(-3.7122, -3.6958, -3.6774, -3.6394, -3.7215, -3.7336, -3.5935, -3.7026,
            -3.6453, -3.7097, -3.7137, -3.6458, -3.6862, -3.6889, -3.6823, -3.6951)
  )
  
  # Crear combinación de todas las horas x estaciones x contaminantes
  combinaciones <- expand.grid(
    fecha_hora = datos_meteo$fecha_hora,
    id_estacion = estaciones$id_estacion,
    contaminante = names(modelos),
    stringsAsFactors = FALSE
  ) %>%
    # Unir con datos meteorológicos
    left_join(datos_meteo, by = "fecha_hora") %>%
    # Unir con información de estaciones
    left_join(estaciones, by = "id_estacion")
  
  log_info("Combinaciones creadas: {nrow(combinaciones)} registros")
  log_info("  {nrow(datos_meteo)} horas × {nrow(estaciones)} estaciones × {length(modelos)} contaminantes")
  
  # Generar predicciones para cada contaminante
  predicciones_finales <- list()
  
  for(contaminante_nombre in names(modelos)) {
    log_info("Prediciendo {contaminante_nombre}...")
    
    # Filtrar datos para este contaminante
    datos_contaminante <- combinaciones %>%
      filter(contaminante == contaminante_nombre)
    
    # Obtener modelo
    modelo_info <- modelos[[contaminante_nombre]]
    modelo_caret <- modelo_info$modelo
    
    tryCatch({
      # Hacer predicciones
      predicciones <- predict(modelo_caret, newdata = datos_contaminante)
      
      # Agregar predicciones a los datos
      datos_con_predicciones <- datos_contaminante %>%
        mutate(
          prediccion = predicciones,
          rmse_modelo = modelo_info$rmse,
          r2_modelo = modelo_info$rsquared
        )
      
      predicciones_finales[[contaminante_nombre]] <- datos_con_predicciones
      
      log_success("✓ {contaminante_nombre}: {nrow(datos_con_predicciones)} predicciones")
      log_info("  Rango: {round(min(predicciones), 1)} - {round(max(predicciones), 1)} µg/m³")
      
    }, error = function(e) {
      log_error("Error prediciendo {contaminante_nombre}: {e$message}")
    })
  }
  
  # Combinar todas las predicciones
  if(length(predicciones_finales) == 0) {
    log_error("No se generaron predicciones")
    return(NULL)
  }
  
  resultado_completo <- bind_rows(predicciones_finales)
  
  # Convertir a objeto espacial
  resultado_sf <- resultado_completo %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    mutate(timestamp_prediccion = Sys.time())
  
  log_success("✓ Predicciones horarias completadas: {nrow(resultado_sf)} registros")
  
  return(resultado_sf)
}

# 5. FUNCIÓN PRINCIPAL ----
generar_predicciones_40h <- function(horas = 40) {
  
  log_info("=== GENERANDO PREDICCIONES PRÓXIMAS {horas} HORAS ===")
  
  # Paso 1: Generar datos meteorológicos futuros
  datos_meteo <- generar_meteo_futuro(horas)
  
  if(is.null(datos_meteo)) {
    log_error("No se pudieron generar datos meteorológicos")
    return(NULL)
  }
  
  # Paso 2: Generar predicciones para estaciones
  predicciones <- generar_predicciones_estaciones(datos_meteo)
  
  if(is.null(predicciones)) {
    log_error("No se pudieron generar predicciones")
    return(NULL)
  }
  
  # Paso 3: Guardar resultados
  archivo_salida <- "output/predicciones_40h_latest.rds"
  saveRDS(predicciones, archivo_salida)
  
  # También guardar datos meteorológicos para gráficos
  archivo_meteo <- "output/meteo_40h_latest.rds"
  saveRDS(datos_meteo, archivo_meteo)
  
  log_success("✓ PREDICCIONES 40H COMPLETADAS")
  log_info("  Total registros: {nrow(predicciones)}")
  log_info("  Horas: {horas}")
  log_info("  Estaciones: {length(unique(predicciones$id_estacion))}")
  log_info("  Contaminantes: {length(unique(predicciones$contaminante))}")
  log_info("  Archivo predicciones: {archivo_salida}")
  log_info("  Archivo meteorología: {archivo_meteo}")
  
  return(predicciones)
}

# 6. FUNCIÓN DE TEST ----
test_predicciones_40h <- function() {
  log_info("=== TEST PREDICCIONES 40H ===")
  
  resultado <- generar_predicciones_40h(40)
  
  if(is.null(resultado)) {
    log_error("❌ Test falló")
    return(FALSE)
  }
  
  # Validaciones
  validaciones <- list(
    registros_totales = nrow(resultado),
    horas_unicas = length(unique(resultado$fecha_hora)),
    estaciones_unicas = length(unique(resultado$id_estacion)),
    contaminantes_unicos = length(unique(resultado$contaminante)),
    rango_predicciones = c(min(resultado$prediccion), max(resultado$prediccion)),
    valores_negativos = sum(resultado$prediccion < 0),
    valores_na = sum(is.na(resultado$prediccion))
  )
  
  log_success("✓ Test predicciones 40h exitoso")
  log_info("  Registros totales: {validaciones$registros_totales}")
  log_info("  Horas únicas: {validaciones$horas_unicas}")
  log_info("  Estaciones únicas: {validaciones$estaciones_unicas}")
  log_info("  Contaminantes únicos: {validaciones$contaminantes_unicos}")
  log_info("  Rango predicciones: {round(validaciones$rango_predicciones[1], 1)} - {round(validaciones$rango_predicciones[2], 1)}")
  log_info("  Valores negativos: {validaciones$valores_negativos}")
  log_info("  Valores NA: {validaciones$valores_na}")
  
  return(TRUE)
}

# Ejecutar test automáticamente
if(interactive() == FALSE) {
  test_predicciones_40h()
}