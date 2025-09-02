# SCRIPT 01b: INGESTA MASIVA DE DATOS HISTÓRICOS 
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
library(purrr)
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
  query_existentes <- "SELECT DISTINCT EXTRACT(YEAR FROM fecha_hora) as ano, EXTRACT(MONTH FROM fecha_hora) as mes FROM fact_mediciones"
  datos_existentes_dt <- setDT(dbGetQuery(db_conn, query_existentes))
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
  
  ficheros_historicos_dt <- map_dfr(nodos_descarga, function(nodo) {
    ano_texto <- nodo |> html_element("p.info-title") |> html_text(trim = TRUE)
    url_relativo <- nodo |> html_element("a") |> html_attr("href")
    
    if (!is.na(ano_texto) && !is.na(url_relativo) && nzchar(ano_texto)) {
      data.frame(ano = as.integer(ano_texto), url = url_relativo)
    } else {
      NULL
    }
  })
  
  enlace_actual_rel <- pagina_html |> html_element("a[href*='calidad-aire-horario.csv']") |> html_attr("href")
  
  if (!is.na(enlace_actual_rel)) {
    ficheros_a_procesar_dt <- rbindlist(
      list(data.table(ano = year(today()), url = enlace_actual_rel), ficheros_historicos_dt),
      use.names = TRUE, fill = TRUE
    )
  } else {
    ficheros_a_procesar_dt <- setDT(ficheros_historicos_dt)
  }
  
  ficheros_a_procesar_dt[, url := xml2::url_absolute(url, portal_url)]
  ficheros_a_procesar_dt <- unique(ficheros_a_procesar_dt, by = "url")
  
  log_info("Se encontraron {nrow(ficheros_a_procesar_dt)} ficheros de datos únicos para procesar.")

  # CARGAR TABLAS DE DIMENSIONES EN MEMORIA
  log_info("Cargando tabla de dimensiones 'dim_estaciones' desde la BBDD...")
  dim_estaciones <- st_read(db_conn, "dim_estaciones")
  setDT(dim_estaciones)
  
  mapa_meses <- setNames(1:12, c("ene", "feb", "mar", "abr", "may", "jun", "jul", "ago", "sep", "oct", "nov", "dic"))

  # BUCLE PRINCIPAL DE PROCESAMIENTO
  for (i in 1:nrow(ficheros_a_procesar_dt)) {
    ano_actual_fichero <- ficheros_a_procesar_dt$ano[i]
    url_fichero <- ficheros_a_procesar_dt$url[i]
    
    if (grepl("\\.zip$", url_fichero, ignore.case = TRUE)) {
      temp_dir <- tempfile()
      dir.create(temp_dir)
      temp_zip_path <- file.path(temp_dir, "data.zip")
      log_info("--- Procesando ZIP del año {ano_actual_fichero}: {basename(url_fichero)} ---")
      
      tryCatch({
        request(url_fichero) |> req_perform(path = temp_zip_path)
        utils::unzip(temp_zip_path, exdir = temp_dir)
        ficheros_csv <- list.files(temp_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
        
        for (fichero_csv_path in ficheros_csv) {
          mes_actual_abr <- sub("_mo\\d{2}\\.csv$", "", basename(fichero_csv_path))
          mes_actual_num <- mapa_meses[mes_actual_abr]
          
          if (nrow(datos_existentes_dt[ano == ano_actual_fichero & mes == mes_actual_num]) > 0) {
            log_info("OMITIENDO: {mes_actual_abr}/{ano_actual_fichero} ya existe.")
            next
          }
          
          log_info("PROCESANDO: {mes_actual_abr}/{ano_actual_fichero}...")
          dbExecute(db_conn, glue("DELETE FROM fact_mediciones WHERE EXTRACT(YEAR FROM fecha_hora) = {ano_actual_fichero} AND EXTRACT(MONTH FROM fecha_hora) = {mes_actual_num};"))
          procesar_y_cargar_lote(fread(fichero_csv_path), db_conn, dim_estaciones)
        }
      }, error = function(e) {log_error("Fallo procesando ZIP {basename(url_fichero)}: {e$message}")},
         finally = {unlink(temp_dir, recursive = TRUE)})
         
    } else if (grepl("\\.csv$", url_fichero, ignore.case = TRUE)) {
      log_info("--- Procesando CSV único: {basename(url_fichero)} ---")
      tryCatch({
        datos_crudos <- fread(url_fichero, sep = ";", dec = ",", encoding = "UTF-8")
        if (nrow(datos_crudos) == 0) {log_warn("El CSV está vacío."); next}
        
        # Para un CSV de año en curso, puede contener varios meses. Los procesamos todos.
        meses_en_fichero <- unique(datos_crudos$MES)
        for (mes_del_fichero in meses_en_fichero) {
          
          if (nrow(datos_existentes_dt[ano == ano_actual_fichero & mes == mes_del_fichero]) > 0) {
            log_info("OMITIENDO: Mes {mes_del_fichero}/{ano_actual_fichero} ya existe.")
            next
          }
          log_info("PROCESANDO: Mes {mes_del_fichero}/{ano_actual_fichero}...")
          dbExecute(db_conn, glue("DELETE FROM fact_mediciones WHERE EXTRACT(YEAR FROM fecha_hora) = {ano_actual_fichero} AND EXTRACT(MONTH FROM fecha_hora) = {mes_del_fichero};"))
          procesar_y_cargar_lote(datos_crudos[MES == mes_del_fichero], db_conn, dim_estaciones)
        }
      }, error = function(e) {log_error("Fallo procesando CSV directo {basename(url_fichero)}: {e$message}")})
    }
  }

}, error = function(e_main) {
  log_error("Error fatal en el script: {e_main$message}")
}, finally = {
  log_info("--- PROCESO DE INGESTA HISTÓRICA COMPLETADO ---")
  if (exists("db_conn") && R6::is.R6(db_conn) && dbIsValid(db_conn)) {
    DBI::dbDisconnect(db_conn)
  }
})
