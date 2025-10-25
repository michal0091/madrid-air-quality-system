# SCRIPT 03: INGENIERÍA DE CARACTERÍSTICAS (CREACIÓN DE PREDICTORES)
# --------------------------------------------------------------------
# Objetivo: Enriquecer la tabla 'dim_estaciones' con covariables espaciales
# para su uso en modelos de Machine Learning.
# --------------------------------------------------------------------

# --- 1. CARGA DE LIBRERÍAS Y ENTORNO ----
renv::load()
library(DBI)
library(RPostgres)
library(sf)
library(data.table) 
library(elevatr)    # Para obtener datos de altitud (DEM)
library(terra)      # Para procesar los datos raster de altitud
library(osmdata)    # Para obtener datos de OpenStreetMap (carreteras, etc.)
library(logger)
library(glue)

# --- CONEXIÓN A LA BASE DE DATOS ----
db_conn <- NULL 
log_info("Estableciendo conexión con la base de datos PostgreSQL...")

tryCatch({
  db_conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"), port = Sys.getenv("DB_PORT"),
    dbname = Sys.getenv("DB_NAME"), user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
  log_success("Conexión a la base de datos establecida.")


  # --- 2. CARGA Y PREPARACIÓN DE DATOS ----
  
  # --- 1. CARGAR DATOS BASE DE ESTACIONES ---
  log_info("Cargando la tabla 'dim_estaciones' desde la base de datos...")
  estaciones_sf <- st_read(db_conn, "dim_estaciones")
  log_success("Se cargaron {nrow(estaciones_sf)} estaciones.")

  # --- 2. PREDICTOR 1: ALTITUD ---
  log_info("Obteniendo datos de altitud para cada estación...")
  dem_madrid <- get_elev_raster(locations = estaciones_sf, z = 12, prj = "EPSG:4326")
  
  # Extraemos el valor de altitud para cada punto (estación)
  altitud_estaciones <- terra::extract(dem_madrid, estaciones_sf, fun = mean, na.rm = TRUE)
  
  setDT(estaciones_sf)
  estaciones_sf[, altitud_m := altitud_estaciones]

  log_success("Predictor 'altitud_m' añadido.")

  # --- 3. PREDICTOR 2: DISTANCIA A CARRETERAS PRINCIPALES ---
  log_info("Obteniendo datos de carreteras de OpenStreetMap...")
  estaciones_sf <- st_as_sf(estaciones_sf)
  bbox_madrid <- st_bbox(estaciones_sf)
  
  q_carreteras <- opq(bbox = bbox_madrid) |>
    add_osm_feature(key = "highway", value = c("motorway", "trunk", "primary", "secondary"))
  
  resp_carreteras <- osmdata_sf(q_carreteras)
  carreteras_sf <- resp_carreteras$osm_lines
  
  if (nrow(carreteras_sf) > 0) {
    log_info("Calculando distancia a carretera (método st_nearest_feature optimizado)...")
    
    # 1. Encontrar el ÍNDICE de la carretera más cercana
    nearest_idx <- st_nearest_feature(estaciones_sf, carreteras_sf)
    
    # 2. Calcular la distancia SOLAMENTE a esa carretera
    dist_min <- st_distance(
      estaciones_sf, 
      carreteras_sf[nearest_idx, ], 
      by_element = TRUE
    )
    
    # Asignar por referencia
    setDT(estaciones_sf)
    estaciones_sf[, dist_carretera_m := as.numeric(dist_min)]
    estaciones_sf <- st_as_sf(estaciones_sf)
    log_success("Predictor 'dist_carretera_m' añadido.")
    
  } else {
    log_warn("No se encontraron carreteras de OSM en el área. Omitiendo este predictor.")
  }
  
  # --- 4. PREDICTOR 3: TIPO DE USO DE SUELO ---
  log_info("Obteniendo datos de uso de suelo (zonas verdes) de OpenStreetMap...")
  q_zonas_verdes <- opq(bbox = bbox_madrid) |>
    add_osm_feature(key = "leisure", value = c("park", "garden")) |>
    add_osm_feature(key = "landuse", value = c("forest", "grass"))
    
  resp_zonas_verdes <- osmdata_sf(q_zonas_verdes)
  zonas_verdes_sf <- resp_zonas_verdes$osm_polygons
  
  if (nrow(zonas_verdes_sf) > 0) {
    # Hacemos un join espacial para ver si una estación está DENTRO de una zona verde.
    join_result <- st_join(estaciones_sf, zonas_verdes_sf[, "leisure", drop = FALSE], join = st_intersects)
    setDT(estaciones_sf)
    estaciones_sf[, es_zona_verde := fifelse(!is.na(join_result$leisure), 1, 0)]
    log_success("Predictor 'es_zona_verde' añadido. {sum(estaciones_sf$es_zona_verde)} estaciones en zonas verdes.")
  } else {
    log_warn("No se encontraron zonas verdes de OSM en el área. Omitiendo este predictor.")
  }

  # --- 5. ACTUALIZAR LA TABLA EN LA BASE DE DATOS ---
  log_info("Actualizando la tabla 'dim_estaciones' en la base de datos con los nuevos predictores...")
  # st_write funciona perfectamente con objetos sf/data.table
  estaciones_sf <- st_as_sf(estaciones_sf)
  st_write(estaciones_sf, dsn = db_conn, layer = "dim_estaciones", delete_layer = TRUE)
  log_success("¡Tabla 'dim_estaciones' actualizada con éxito!")

}, error = function(e) {
  log_error("Fallo durante la creación de predictores: {e$message}")
}, finally = {
  log_info("Cerrando conexión a la base de datos.")
  # [FIX 2] Comprobación de conexión robusta
  if (!is.null(db_conn) && R6::is.R6(db_conn) && dbIsValid(db_conn)) {
    DBI::dbDisconnect(db_conn)
  }
})

log_info("--- PROCESO DE CREACIÓN DE PREDICTORES FINALIZADO ---")