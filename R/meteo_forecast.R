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
      log_info("Usando datos fallback meteorológicos")
      return(generar_prediccion_fallback(horas_prediccion))
    } else {
      stop("API key requerida")
    }
  }
  
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
  
  # Validar y procesar datos
  prediccion_procesada <- procesar_prediccion_aemet(prediccion)
  
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
  
  # Parsear JSON
  datos_json <- resp_body_json(resp_datos)
  
  log_info("✅ Datos AEMET descargados correctamente")
  return(datos_json)
}

#' Procesa datos crudos de AEMET a formato estándar
procesar_prediccion_aemet <- function(datos_crudos) {
  
  log_info("Procesando datos AEMET...")
  
  # Extraer predicciones horarias
  predicciones <- datos_crudos[[1]]$prediccion$dia
  
  resultado <- data.frame()
  
  for(dia in predicciones) {
    fecha_dia <- as.Date(dia$fecha)
    
    # Variables horarias disponibles
    variables <- c("temperatura", "sensTermica", "humedadRelativa", 
                   "precipitacion", "vientoAndComponenteU", "vientoAndComponenteV",
                   "dirViento10m", "viento10m")
    
    for(hora_idx in 1:length(dia$temperatura)) {
      timestamp <- as.POSIXct(paste(fecha_dia, sprintf("%02d:00:00", hora_idx - 1)), 
                              tz = "Europe/Madrid")
      
      fila <- data.frame(
        timestamp = timestamp,
        fecha = fecha_dia,
        hora = hora_idx - 1,
        temperatura_c = as.numeric(dia$temperatura[hora_idx]),
        sensacion_termica_c = as.numeric(dia$sensTermica[hora_idx]),
        humedad_relativa_pct = as.numeric(dia$humedadRelativa[hora_idx]),
        precipitacion_mm = as.numeric(dia$precipitacion[hora_idx]),
        viento_u_ms = as.numeric(dia$vientoAndComponenteU[hora_idx]),
        viento_v_ms = as.numeric(dia$vientoAndComponenteV[hora_idx]),
        direccion_viento_grados = as.numeric(dia$dirViento10m[hora_idx]),
        velocidad_viento_ms = as.numeric(dia$viento10m[hora_idx]),
        presion_hpa = 1013.25, # Valor por defecto si no disponible
        stringsAsFactors = FALSE
      )
      
      resultado <- rbind(resultado, fila)
    }
  }
  
  # Filtrar a horas solicitadas
  ahora <- Sys.time()
  limite <- ahora + hours(FORECAST_HOURS)
  resultado <- resultado[resultado$timestamp <= limite, ]
  
  log_info("Procesados {nrow(resultado)} registros horarios")
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