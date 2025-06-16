# SCRIPT 01b: INGESTA MASIVA DE DATOS HISTÓRICOS
# Objetivo: Descargar, procesar y cargar todos los ficheros de datos horarios históricos.

renv::load()
library(httr2)
library(rvest) # Para web scraping
library(data.table)
library(lubridate)
library(sf)
library(DBI)
library(RPostgres)
library(logger)
library(glue)

# --- CONFIGURACIÓN (LOGGING Y BBDD) ---
log_appender(appender_tee("logs/collect_data.log"))
log_info("--- INICIO DEL PROCESO DE RECOLECCIÓN DE DATOS ---")

tryCatch(
  {
    log_info("Estableciendo conexión con la base de datos PostgreSQL...")

    db_conn <- DBI::dbConnect(
      RPostgres::Postgres(),
      host = Sys.getenv("DB_HOST"),
      port = Sys.getenv("DB_PORT"),
      dbname = Sys.getenv("DB_NAME"),
      user = Sys.getenv("DB_USER"),
      password = Sys.getenv("DB_PASSWORD")
    )

    log_success("Conexión a la base de datos establecida correctamente.")
  },
  error = function(e) {
    log_error("No se pudo conectar a la base de datos.")
    log_error(e$message)
    stop("Error de conexión a la BBDD. Abortando script.")
  }
)

# --- 1. OBTENER ENLACES DE DESCARGA (WEB SCRAPING) ---
tryCatch({
  log_info("Estableciendo conexión con la base de datos PostgreSQL...")
  db_conn <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"), port = Sys.getenv("DB_PORT"),
    dbname = Sys.getenv("DB_NAME"), user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
  log_success("Conexión a la base de datos establecida.")

  # --- 1. OBTENER ENLACES DE DESCARGA (WEB SCRAPING) ---
  portal_url <- "https://datos.madrid.es/portal/site/egob/menuitem.c05c1f754a33a9fbe4b2e4b284f1a5a0/?vgnextoid=f3c0f7d512273410VgnVCM2000000c205a0aRCRD"
  log_info("Haciendo scraping en la página del portal: {portal_url}")

  pagina_html <- read_html(portal_url)
  enlaces_relativos <- pagina_html |>
    html_elements("a[href$='.zip']") |>
    html_attr("href")
  enlaces_absolutos <- xml2::url_absolute(enlaces_relativos, portal_url)

  log_info("Se encontraron {length(enlaces_absolutos)} ficheros ZIP anuales para descargar.")

  # --- 2. BUCLE ANIDADO DE DESCARGA, PROCESAMIENTO Y CARGA ---
  log_info("Cargando tabla de dimensiones de estaciones desde la BBDD...")
  log_info("Cargando tablas de dimensiones (estaciones, magnitudes) desde la BBDD...")
  dim_estaciones <- setDT((st_read(db_conn, "dim_estaciones")))
  dim_magnitudes <- setDT(dbReadTable(db_conn, "dim_magnitudes"))

  # BUCLE EXTERIOR: Itera sobre cada fichero ZIP (un ZIP por año)
  for (url_fichero_zip in enlaces_absolutos) {
    log_info("--- Procesando ZIP: {basename(url_fichero_zip)} ---")

    temp_zip <- tempfile(fileext = ".zip")

    tryCatch({
      # Descargar el ZIP a un fichero temporal
      request(url_fichero_zip) |> req_perform(path = temp_zip)

      # Listar todos los ficheros DENTRO del ZIP
      lista_ficheros_internos <- utils::unzip(temp_zip, list = TRUE)

      # Filtrar para quedarnos solo con los ficheros .csv
      ficheros_csv_en_zip <- lista_ficheros_internos$Name[grepl("\\.csv$", lista_ficheros_internos$Name, ignore.case = TRUE)]

      log_info("Se encontraron {length(ficheros_csv_en_zip)} ficheros CSV dentro del ZIP.")

      # BUCLE INTERIOR: Itera sobre cada fichero CSV (un CSV por mes)
      for (fichero_csv_path in ficheros_csv_en_zip) {
        tryCatch(
          {
            log_info("Procesando fichero mensual: {fichero_csv_path}")

            # Leer el CSV directamente desde el ZIP sin descomprimir todo
            datos_crudos <- fread(unzip(temp_zip, files = fichero_csv_path))

            # Lógica de transformación de ancho a largo (reutilizada)
            datos_largos <- melt(datos_crudos,
              id.vars = c("PROVINCIA", "MUNICIPIO", "ESTACION", "MAGNITUD", "PUNTO_MUESTREO", "ANO", "MES", "DIA"),
              measure.vars = patterns("^H", "^V"),
              variable.name = "hora_indice",
              value.name = c("valor", "valido")
            )

            datos_largos <- datos_largos[valido == "V"]
            datos_largos[, `:=`(
              hora = as.integer(hora_indice) - 1,
              valor = as.numeric(sub(",", ".", valor, fixed = TRUE))
            )]
            datos_largos[, fecha := make_datetime(ANO, MES, DIA, hora, tz = "Europe/Madrid")]

            setnames(datos_largos, c("ESTACION", "MAGNITUD"), c("id_estacion", "id_magnitud"))

            # Unimos con las dimensiones para obtener los IDs
            datos_con_ids <- merge(datos_largos, dim_estaciones[, .(codigo_largo, id_estacion)], by = "id_estacion") # Solo para asegurar que existe
            datos_con_ids <- merge(datos_con_ids, dim_magnitudes[, .(id_magnitud)], by = "id_magnitud")

            # Creamos la tabla final de hechos, limpia y con IDs
            fact_mediciones_dt <- datos_con_ids[, .(
              id_estacion, # La clave de la estación
              id_magnitud, # La clave de la magnitud
              fecha_hora = fecha,
              valor_medido = valor
            )]

            # Escribimos LA TABLA DE HECHOS en la base de datos
            dbWriteTable(db_conn, "fact_mediciones", fact_mediciones_dt, append = TRUE)
          },
          error = function(e_mes) {
            log_error("FALLO en el fichero mensual {fichero_csv_path}: {e_mes$message}")
          }
        )
      } # Fin del bucle interior (meses)
    }, error = function(e_zip) {
      log_error("FALLO GRAVE al procesar el ZIP {basename(url_fichero_zip)}: {e_zip$message}")
    }, finally = {
      # Limpiamos el fichero temporal del ZIP
      unlink(temp_zip)
    })
  } # Fin del bucle exterior (años)
}, error = function(e_main) {
  log_error("Error fatal en el script: {e_main$message}")
}, finally = {
  log_info("--- PROCESO DE INGESTA HISTÓRICA COMPLETADO ---")
  if (exists("db_conn")) DBI::dbDisconnect(db_conn)
})
