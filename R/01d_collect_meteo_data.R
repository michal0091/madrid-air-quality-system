# --- SCRIPT 01d: INGESTA DE DATOS METEOROLÓGICOS DE AEMET ---
# --------------------------------------------------------------------
# Objetivo: Descargar datos diarios de TODAS las estaciones meteorológicas
# de la AEMET en Madrid y almacenarlos en la base de datos.
# --------------------------------------------------------------------

# --- 1. CARGA DE LIBRERÍAS Y ENTORNO ----
renv::load()
library(DBI)
library(RPostgres)
library(data.table)
library(climaemet)
library(sf)
library(lubridate)
library(purrr)
library(logger)
library(glue)

# --- 2. CONFIGURACIÓN ----
log_appender(appender_tee(glue("logs/aemet_load_{format(Sys.Date(), '%Y%m%d')}.log")))
log_info("--- INICIO DEL PROCESO DE INGESTA DE DATOS AEMET ---")

# --- 3. SCRIPT PRINCIPAL ----
tryCatch({
  # --- Configurar API Key de forma segura ---
  aemet_api_key(Sys.getenv("AEMET_API_KEY"), install = TRUE, overwrite=TRUE) # 'install = TRUE' la guarda para futuras sesiones

  log_info("Estableciendo conexión con la base de datos PostgreSQL...")
  db_conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"), port = Sys.getenv("DB_PORT"),
    dbname = Sys.getenv("DB_NAME"), user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
  log_success("Conexión a la base de datos establecida.")

  # --- OBTENER CATÁLOGO DE ESTACIONES DE AEMET EN MADRID ---
  log_info("Obteniendo catálogo completo de estaciones de AEMET...")
  estaciones_aemet_sf <- aemet_stations()
  setDT(estaciones_aemet_sf)

  estaciones_madrid_dt <- estaciones_aemet_sf[provincia == "MADRID"]
  log_success("Se encontraron {nrow(estaciones_madrid_dt)} estaciones de AEMET en Madrid.")
  
  estacion_retiro <- estaciones_madrid_dt[nombre %like% "RETIRO"]

  if (nrow(estacion_retiro) == 0) {
    log_error("No se encontró la estación 'RETIRO' en el catálogo de Madrid. Verifique el nombre o el catálogo.")
    stop("Estación 'RETIRO' no encontrada.")
    
  }

  # --- DESCARGAR DATOS HISTÓRICOS ---
  ano_inicio <- 2000 
  ano_fin <- year(today())
  
  log_info("Iniciando descarga de datos para la estación de {estacion_retiro$nombre}...")
  meteo_bruto_dt <- aemet_daily_period(station = estacion_retiro$indicativo, start = ano_inicio, end = ano_fin)
  setDT(meteo_bruto_dt)
 
  log_success("Descarga completada. Se obtuvieron {nrow(meteo_bruto_dt)} registros en total.")

  # --- PROCESAMIENTO Y CARGA ---
  log_info("Limpiando y preparando datos para la carga...")
  
  meteo_limpio_dt <- meteo_bruto_dt[, .(
    id_estacion_aemet = indicativo,
    fecha = as.Date(fecha),
    temp_media_c = tmed,
    precipitacion_mm = prec,
    vel_viento_media_ms = velmedia
  )]
  
  meteo_limpio_dt <- na.omit(meteo_limpio_dt, cols = c("temp_media_c", "precipitacion_mm", "vel_viento_media_ms"))

  db_table_name <- "fact_meteo_diaria"
  log_info("Escribiendo {nrow(meteo_limpio_dt)} registros en '{db_table_name}'...")
  dbWriteTable(db_conn, db_table_name, meteo_limpio_dt, overwrite = TRUE)
  log_success("Tabla '{db_table_name}' creada/actualizada con éxito.")

}, error = function(e) {
  log_error("Fallo durante la ingesta de datos meteorológicos de AEMET: {e$message}")
}, finally = {
  log_info("Cerrando conexión a la base de datos.")
  if (exists("db_conn") && R6::is.R6(db_conn) && dbIsValid(db_conn)) {
    DBI::dbDisconnect(db_conn)
  }
})

log_info("--- PROCESO DE INGESTA AEMET FINALIZADO ---")