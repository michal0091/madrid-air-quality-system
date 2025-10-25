# SCRIPT 00: CREACIÓN DE TABLAS DE DIMENSIONES
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

db_conn <- NULL


tryCatch({
  log_info("Estableciendo conexión con la base de datos PostgreSQL...")
  
  # Formato de llamada a función multi-línea (Guía de Estilo)
  db_conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"), 
    port = Sys.getenv("DB_PORT"),
    dbname = Sys.getenv("DB_NAME"), 
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
  log_success("Conexión a la base de datos establecida.")

  # --- OBTENER Y PROCESAR DATOS DE ESTACIONES DESDE EL CSV ---
  catalogo_url <- "https://datos.madrid.es/egob/catalogo/212629-1-estaciones-control-aire.csv"
  log_info("Descargando catálogo maestro de estaciones desde: {catalogo_url}")
  
  # Usamos fread por su rendimiento
  estaciones_dt <- fread(catalogo_url, sep = ";", dec = ",", encoding = "UTF-8")
  log_info("Catálogo de estaciones descargado con {nrow(estaciones_dt)} registros.")

  setnames(
    estaciones_dt,
    old = c(
      "CODIGO", "CODIGO_CORTO", "ESTACION", "DIRECCION", 
      "COORDENADA_X_ETRS89", "COORDENADA_Y_ETRS89", "ALTITUD", "NOM_TIPO"
    ),
    new = c(
      "codigo_largo", "id_estacion", "nombre_estacion", "direccion", 
      "utm_x", "utm_y", "altitud", "tipo_estacion"
    )
  )

  # Aseguramos tipos numéricos, también por referencia (eficiente)
  estaciones_dt[, `:=`(
    utm_x = as.numeric(utm_x), 
    utm_y = as.numeric(utm_y)
  )]

  log_info("Creando objeto espacial 'sf' y escribiendo en 'dim_estaciones'...")
  
  estaciones_dt[!is.na(utm_x)] |>
    st_as_sf(
      coords = c("utm_x", "utm_y"),
      crs = 25830 # CRS para ETRS89 UTM Zona 30N
    ) |>
    st_transform(crs = 4326) |> # Transformamos a WGS84 (lat/lon)
    st_write(
      dsn = db_conn, 
      layer = "dim_estaciones", 
      delete_layer = TRUE, 
      quiet = TRUE
    )
  
  log_success("TABLA 'dim_estaciones' CREADA Y POBLADA CON ÉXITO.")

  # --- PROCESAR Y CARGAR DIM_MAGNITUDES ---
  log_info("Creando tabla 'dim_magnitudes' desde datos estáticos...")
  magnitudes_dt <- data.table(
    id_magnitud = c(1, 6, 7, 8, 9, 10, 12, 14, 20, 30, 35, 42, 44, 431),
    descripcion = c(
      "Dióxido de Azufre", "Monóxido de Carbono", "Monóxido de Nitrógeno", 
      "Dióxido de Nitrógeno", "Partículas < 2.5 µm", "Partículas < 10 µm", 
      "Óxidos de Nitrógeno", "Ozono", "Tolueno", "Benceno", 
      "Etilbenceno", "Metaxileno", "Paraxileno", "Ortocileno"
    ),
    unidad_medida = c(
      "µg/m³", "mg/m³", "µg/m³", "µg/m³", "µg/m³", "µg/m³", 
      "µg/m³", "µg/m³", "µg/m³", "µg/m³", "µg/m³", "µg/m³", 
      "µg/m³", "µg/m³"
    )
  )

  log_info("Escribiendo {nrow(magnitudes_dt)} magnitudes en la tabla 'dim_magnitudes'...")
  dbWriteTable(db_conn, "dim_magnitudes", magnitudes_dt, overwrite = TRUE)
  log_success("Tabla 'dim_magnitudes' creada y poblada con éxito.")

}, error = function(e) {
  log_error("Fallo durante el setup de la BBDD: {e$message}")
  
}, finally = {
  log_info("Cerrando conexión a la base de datos.")
  if (!is.null(db_conn) && R6::is.R6(db_conn) && dbIsValid(db_conn)) {
    DBI::dbDisconnect(db_conn)
  }
})

log_info("--- PROCESO FINALIZADO ---")