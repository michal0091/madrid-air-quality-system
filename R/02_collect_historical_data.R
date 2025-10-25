# SCRIPT 02: INGESTA MASIVA DE DATOS HISTÓRICOS 
# --------------------------------------------------------------------
# Realiza un scraping estructurado del portal de datos para obtener los enlaces
# a los ficheros históricos. Procesa de forma incremental, cargando únicamente
# los meses/años que no existan previamente en la base de datos.
# Es idempotente a nivel mensual y gestiona la limpieza de ficheros temporales.
# --------------------------------------------------------------------

# --- 1. CARGA DE LIBRERÍAS Y ENTORNO ----
renv::load()
library(httr2)
library(rvest)
library(data.table)
library(lubridate)
library(sf)
library(DBI)
library(RPostgres)
library(logger)
library(glue)

# --- Carga de funciones auxiliares ---
source("R/utils.R")


# --- 2. CONFIGURACIÓN DE LOGGING ----
log_appender(appender_tee(glue("logs/historical_load_{format(Sys.Date(), '%Y%m%d')}.log")))
log_info("--- INICIO DEL PROCESO DE INGESTA HISTÓRICA ---")


# --- 3. SCRIPT PRINCIPAL DE EJECUCIÓN ---
tryCatch({
  log_info("Estableciendo conexión con la base de datos PostgreSQL...")
  db_conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"), port = Sys.getenv("DB_PORT"),
    dbname = Sys.getenv("DB_NAME"), user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
  log_success("Conexión a la base de datos establecida.")

  # OBTENER ESTADO ACTUAL DE LA BASE DE DATOS
  log_info("Comprobando qué datos de mes/año ya existen en 'fact_mediciones'...")
  query_existentes <- 
    "SELECT 
      DISTINCT
      EXTRACT(YEAR FROM fecha_hora) as ano, 
      EXTRACT(MONTH FROM fecha_hora) as mes 
      FROM fact_mediciones"
  
  datos_existentes_dt <- dbGetQuery(db_conn, query_existentes) |> 
    setDT()
  
  setkey(datos_existentes_dt, ano, mes) 
  
  if (nrow(datos_existentes_dt) > 0) {
      log_info("Se encontraron {nrow(datos_existentes_dt)} combinaciones de mes/año ya cargadas.")
  } else {
      log_info("La tabla 'fact_mediciones' está vacía. Se procederá a cargar todos los datos.")
  }
  
  # OBTENER FUENTES DE DATOS DISPONIBLES (SCRAPING)
  portal_url <- "https://datos.madrid.es/portal/site/egob/menuitem.c05c1f754a33a9fbe4b2e4b284f1a5a0/?vgnextoid=f3c0f7d512273410VgnVCM2000000c205a0aRCRD"
  log_info("Haciendo scraping ESTRUCTURADO en la página del portal: {portal_url}")
  
  pagina_html <- read_html(portal_url)
  nodos_descarga <- pagina_html |> html_elements(".asociada-list li.asociada-item")
  
  ficheros_historicos_dt <- rbindlist(lapply(nodos_descarga, function(nodo) {
    ano_texto <- nodo |> html_element("p.info-title") |> html_text(trim = TRUE)
    url_relativo <- nodo |> html_element("a") |> html_attr("href")
    
    if (!is.na(ano_texto) && !is.na(url_relativo) && nzchar(ano_texto)) {
      # Devolvemos un data.table (o list) directamente
      data.table(ano = as.integer(ano_texto), url = url_relativo) 
    } else {
      NULL
    }
  }))
  
  enlace_actual_rel <- pagina_html |> html_element("a[href*='calidad-aire-horario.csv']") |> html_attr("href")
  
  # rbindlist es la forma idiomática de data.table para unir
  if (!is.na(enlace_actual_rel)) {
    ficheros_a_procesar_dt <- rbindlist(
      list(data.table(ano = year(today()), url = enlace_actual_rel), ficheros_historicos_dt),
      use.names = TRUE, fill = TRUE
    )
  } else {
    ficheros_a_procesar_dt <- ficheros_historicos_dt
  }
  
  ficheros_a_procesar_dt <- ficheros_a_procesar_dt[, url := xml2::url_absolute(url, portal_url)] |>
    unique(by = "url")
  
  log_info("Se encontraron {nrow(ficheros_a_procesar_dt)} ficheros de datos únicos para procesar.")

  # CARGAR TABLAS DE DIMENSIONES EN MEMORIA
  log_info("Cargando tabla de dimensiones 'dim_estaciones' desde la BBDD...")
  dim_estaciones <- st_read(db_conn, "dim_estaciones") |> setDT()
  
  mapa_meses <- setNames(1:12, c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic"))
  
  # 1. Clasificar el tipo de fichero por referencia (eficiente)
  ficheros_a_procesar_dt[, tipo_fichero := fcase(
    grepl("\\.zip$", url, ignore.case = TRUE), "zip",
    grepl("\\.csv$", url, ignore.case = TRUE), "csv",
    default = "desconocido"
  )]

  # 2. Definir funciones auxiliares
  
  procesar_lote_zip <- function(url_zip, ano_fichero, datos_existentes, mapa_meses, db_conn, dim_estaciones) {
    temp_dir <- tempfile()
    dir.create(temp_dir)
    temp_zip_path <- file.path(temp_dir, "data.zip")
    log_info("--- Procesando ZIP del año {ano_fichero}: {basename(url_zip)} ---")
    
    tryCatch({
      request(url_zip) |> req_perform(path = temp_zip_path)
      utils::unzip(temp_zip_path, exdir = temp_dir)
      ficheros_csv <- list.files(temp_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
      
      for (fichero_csv_path in ficheros_csv) {
        mes_actual_abr <- sub("_mo\\d{2}\\.csv$", "", basename(fichero_csv_path))
        mes_actual_num <- mapa_meses[mes_actual_abr]
        
        if (datos_existentes[.(ano_fichero, mes_actual_num), .N > 0]) {
          log_info("OMITIENDO: {mes_actual_abr}/{ano_fichero} ya existe.")
          next
        }
        
        log_info("PROCESANDO: {mes_actual_abr}/{ano_fichero}...")
        dbExecute(db_conn, glue("DELETE FROM fact_mediciones WHERE EXTRACT(YEAR FROM fecha_hora) = {ano_fichero} AND EXTRACT(MONTH FROM fecha_hora) = {mes_actual_num};"))
        
        procesar_y_cargar_lote_calidad_aire(fread(fichero_csv_path), db_conn, dim_estaciones)
      }
    }, error = function(e) { log_error("Fallo procesando ZIP {basename(url_zip)}: {e$message}") },
       finally = { unlink(temp_dir, recursive = TRUE) })
  }

  procesar_lote_csv <- function(url_csv, ano_fichero, datos_existentes, db_conn, dim_estaciones) {
    log_info("--- Procesando CSV único: {basename(url_csv)} ---")
    tryCatch({
      datos_crudos <- fread(url_csv, sep = ";", dec = ",", encoding = "UTF-8")
      if (nrow(datos_crudos) == 0) { log_warn("El CSV está vacío."); return(NULL) }
      
      meses_en_fichero <- unique(datos_crudos$MES)
      for (mes_del_fichero in meses_en_fichero) {
        
        if (datos_existentes[.(ano_fichero, mes_del_fichero), .N > 0]) {
          log_info("OMITIENDO: Mes {mes_del_fichero}/{ano_fichero} ya existe.")
          next
        }
        log_info("PROCESANDO: Mes {mes_del_fichero}/{ano_fichero}...")
        dbExecute(db_conn, glue("DELETE FROM fact_mediciones WHERE EXTRACT(YEAR FROM fecha_hora) = {ano_fichero} AND EXTRACT(MONTH FROM fecha_hora) = {mes_del_fichero};"))
        
        procesar_y_cargar_lote_calidad_aire(datos_crudos[MES == mes_del_fichero], db_conn, dim_estaciones)
      }
    }, error = function(e) { log_error("Fallo procesando CSV directo {basename(url_csv)}: {e$message}") })
  }

  # 3. Aplicar la lógica "por fila" usando by = 1:nrow()
  ficheros_a_procesar_dt[tipo_fichero != "desconocido", {
    if (tipo_fichero == "zip") {
      procesar_lote_zip(url, ano, datos_existentes_dt, mapa_meses, db_conn, dim_estaciones)
    } else if (tipo_fichero == "csv") {
      procesar_lote_csv(url, ano, datos_existentes_dt, db_conn, dim_estaciones)
    }
    NULL # No necesitamos devolver nada
  }, by = 1:nrow(ficheros_a_procesar_dt)] # El método idiomático de data.table para iterar

}, error = function(e_main) {
  log_error("Error fatal en el script: {e_main$message}")
}, finally = {
  log_info("--- PROCESO DE INGESTA HISTÓRICA COMPLETADO ---")
  if (exists("db_conn") && R6::is.R6(db_conn) && dbIsValid(db_conn)) {
    DBI::dbDisconnect(db_conn)
  }
})