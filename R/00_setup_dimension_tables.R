# SCRIPT 00: CREACIÓN DE TABLAS DE DIMENSIONES (VERSIÓN FINAL Y SIMPLIFICADA)
# Objetivo: Construir la tabla 'dim_estaciones' desde el catálogo oficial en CSV.

renv::load()
library(data.table)
library(httr2)
library(sf)
library(DBI)
library(RPostgres)
library(logger)
library(glue)

# --- CONFIGURACIÓN (LOGGING Y BBDD) ---
log_appender(appender_tee("logs/setup_tables.log"))
log_info("--- INICIO DE LA CREACIÓN DE TABLAS DE DIMENSIONES ---")

tryCatch({
  log_info("Estableciendo conexión con la base de datos PostgreSQL...")
  db_conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"), port = Sys.getenv("DB_PORT"),
    dbname = Sys.getenv("DB_NAME"), user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
  log_success("Conexión a la base de datos establecida.")

  # --- OBTENER Y PROCESAR DATOS DE ESTACIONES DESDE EL CSV ---
  catalogo_url <- "https://datos.madrid.es/egob/catalogo/212629-1-estaciones-control-aire.csv"
  log_info("Descargando catálogo maestro de estaciones desde: {catalogo_url}")
  estaciones_dt <- fread(catalogo_url, sep = ";", dec = ",", encoding = "UTF-8")

  log_info("Catálogo de estaciones descargado con {nrow(estaciones_dt)} registros.")

  # Seleccionamos y renombramos las columnas que nos interesan a un formato estándar
  setnames(estaciones_dt,
    old = c("CODIGO", "CODIGO_CORTO", "ESTACION", "DIRECCION", "COORDENADA_X_ETRS89", "COORDENADA_Y_ETRS89", "ALTITUD", "NOM_TIPO"),
    new = c("codigo_largo", "id_estacion", "nombre_estacion", "direccion", "utm_x", "utm_y", "altitud", "tipo_estacion")
  )

  # Nos aseguramos de que las coordenadas sean numéricas
  estaciones_dt[, `:=`(utm_x = as.numeric(utm_x), utm_y = as.numeric(utm_y))]

  # Creamos el objeto espacial (sf) a partir de las coordenadas UTM
  log_info("Creando objeto espacial 'sf' a partir de coordenadas UTM ETRS89 (CRS:25830)...")
  estaciones_sf <- st_as_sf(estaciones_dt[!is.na(utm_x)],
    coords = c("utm_x", "utm_y"),
    crs = 25830
  ) # CRS para ETRS89 UTM Zona 30N

  # Transformamos a WGS84 (lat/lon) para compatibilidad con Leaflet
  estaciones_sf <- st_transform(estaciones_sf, crs = 4326)

  log_info("Escribiendo {nrow(estaciones_sf)} estaciones en la tabla 'dim_estaciones'...")
  st_write(estaciones_sf, dsn = db_conn, layer = "dim_estaciones", delete_layer = TRUE, quiet = TRUE)
  log_success("TABLA 'dim_estaciones' CREADA Y POBLADA CON ÉXITO.")
}, error = function(e) {
  log_error("Fallo al crear la tabla de estaciones: {e$message}")
}, finally = {
  log_info("Cerrando conexión a la base de datos.")
  if (exists("db_conn") && R6::is.R6(db_conn) && dbIsValid(db_conn)) {
    DBI::dbDisconnect(db_conn)
  }
})

# --- PROCESAR Y CARGAR DIM_MAGNITUDES ---
tryCatch(
  {
    log_info("Creando tabla 'dim_magnitudes' desde el Anexo II del PDF...")
    magnitudes_dt <- data.table(
      id_magnitud = c(1, 6, 7, 8, 9, 10, 12, 14, 20, 30, 35, 42, 44, 431),
      descripcion = c("Dióxido de Azufre", "Monóxido de Carbono", "Monóxido de Nitrógeno", "Dióxido de Nitrógeno", "Partículas < 2.5 µm", "Partículas < 10 µm", "Óxidos de Nitrógeno", "Ozono", "Tolueno", "Benceno", "Etilbenceno", "Metaxileno", "Paraxileno", "Ortocileno"),
      unidad_medida = c("µg/m³", "mg/m³", "µg/m³", "µg/m³", "µg/m³", "µg/m³", "µg/m³", "µg/m³", "µg/m³", "µg/m³", "µg/m³", "µg/m³", "µg/m³", "µg/m³")
    )

    log_info("Escribiendo {nrow(magnitudes_dt)} magnitudes en la tabla 'dim_magnitudes'...")
    dbWriteTable(db_conn, "dim_magnitudes", magnitudes_dt, overwrite = TRUE)
    log_success("Tabla 'dim_magnitudes' creada y poblada con éxito.")
  },
  error = function(e) {
    log_error("Fallo al crear la tabla de magnitudes: {e$message}")
  }
)

log_info("--- PROCESO FINALIZADO ---")
