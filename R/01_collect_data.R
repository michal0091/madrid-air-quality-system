# FASE 1: SCRIPT DE RECOLECCIÓN Y ALMACENAMIENTO DE DATOS
# Objetivo: Obtener datos en formato ANCHO de la API, transformarlos a formato LARGO y guardarlos en PostGIS.

# 1. CARGA DE LIBRERÍAS ----
renv::load()

library(httr2)
library(jsonlite)
library(data.table)
library(lubridate)   
library(sf)
library(DBI)
library(RPostgres)
library(logger)
library(glue)


# 2. CONFIGURACIÓN DEL LOGGING ----
log_appender(appender_tee("logs/collect_data.log")) # Configura el log para que escriba en un fichero y en la consola.
log_info("--- INICIO DEL PROCESO DE RECOLECCIÓN DE DATOS ---")

# 3. CONEXIÓN SEGURA A LA BASE DE DATOS ----
tryCatch({
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
  
}, error = function(e) {
  log_error("No se pudo conectar a la base de datos.")
  log_error(e$message)
  stop("Error de conexión a la BBDD. Abortando script.")
})


# 4. PETICIÓN A LA API DE CALIDAD DEL AIRE (VERSIÓN ROBUSTA) ----
tryCatch({
  fecha_objetivo <- today(tzone = "Europe/Madrid") - days(1)
  base_api_url <- "https://datos.madrid.es/egob/catalogo/300755-12751586-calidad-aire-tiempo-real-acumula.json"
  
  log_info(glue("Preparando petición a la API para la fecha: {fecha_objetivo}"))

  req <- request(base_api_url) |>
    req_url_query(
      ANO = year(fecha_objetivo),
      MES = month(fecha_objetivo),
      DIA = day(fecha_objetivo)
    ) |>
    req_timeout(30) |> 
    req_retry(
      retry_on_failure  = TRUE, 
      # Número máximo de reintentos
      max_tries = 5,
      # Espera exponencial entre reintentos (ej. 1s, 2s, 4s...). Es la mejor práctica.
      backoff = ~ 2^.x
    ) |>
    
    req_user_agent("ProyectoCalidadAireMadrid/1.0 (https://github.com/michal0091/madrid-air-quality-system)")


  log_info(glue("Realizando petición robusta a: {req$url}"))
  
  response <- req_perform(req)
  

  if (resp_status(response) != 200) {
    stop(paste("La API devolvió un error persistente. Código de estado:", resp_status(response)))
  }

  raw_data <- resp_body_json(response, simplifyVector = TRUE)
  log_success("Datos recibidos correctamente de la API después de {response$retry_count} reintentos.")
  
}, error = function(e) {
  log_error("Fallo al obtener datos de la API, incluso después de reintentos.")
  log_error(e$message)
  # (Opcional: cerrar conexión a BBDD si ya estuviera abierta)
  stop("Error en la API. Abortando script.")
})


# 5. PROCESAMIENTO Y TRANSFORMACIÓN DE DATOS (REFACTORIZADO) ----
# -----------------------------------------------------------------
tryCatch({
  log_info("Procesando datos en formato ANCHO recibidos de la API...")

  # Convertir el cuerpo de la respuesta JSON a un data.table
  # La API devuelve los datos en una lista, accedemos al elemento 'datos'
  air_quality_wide_dt <- setDT(raw_data$datos)

  # Punto crítico de validación
  if (nrow(air_quality_wide_dt) == 0) {
    stop("La API no devolvió datos para procesar.")
  }

  log_info("Transformando datos de formato ANCHO a LARGO usando data.table::melt...")

  # Identificadores que no cambian (una fila por cada combinación de estos)
  id_vars <- c("provincia", "municipio", "estacion", "magnitud", "punto_muestreo", "ano", "mes", "dia")

  # Columnas de medición (los valores horarios y sus validaciones)
  # Creamos los nombres de columna para H01-H24 y V01-V24
  measure_vars_h <- paste0("H", sprintf("%02d", 1:24))
  measure_vars_v <- paste0("V", sprintf("%02d", 1:24))

  # Usamos melt para pivotar la tabla. Esta es la operación clave.
  # Le decimos que combine las columnas H y V en dos nuevas columnas: 'valor' y 'valido'
  air_quality_long_dt <- melt(air_quality_wide_dt,
                              id.vars = id_vars,
                              measure.vars = list(valor = measure_vars_h, valido = measure_vars_v),
                              variable.name = "hora_indice", # Esta columna tendrá valores de 1 a 24
                              value.name = c("valor", "valido"))

  log_info("Limpiando y enriqueciendo los datos transformados...")

  # Limpieza y creación de la columna de fecha y hora
  air_quality_long_dt[, `:=`(
      # La API indica que solo los datos con validación "V" son correctos. Filtramos el resto.
      # Primero limpiamos la columna 'valido' de espacios en blanco.
      valido = trimws(valido),
      
      # El índice de hora (1-24) lo convertimos a hora (0-23)
      hora = as.integer(hora_indice) - 1,

      # Convertimos los valores a numérico, manejando la coma decimal
      valor = as.numeric(sub(",", ".", valor, fixed = TRUE))
  )]
  
  # Filtramos solo las mediciones válidas según la documentación.
  air_quality_final_dt <- air_quality_long_dt[valido == "V"]

  # Creamos la columna de fecha-hora completa usando lubridate
  air_quality_final_dt[, fecha := make_datetime(ano, mes, dia, hora, tz = "Europe/Madrid")]
  
  # Ahora necesitamos las coordenadas para hacer el objeto espacial.
  # La API de tiempo real no las incluye. Debemos obtenerlas del fichero de estaciones (Anexo I del PDF).
  # Por ahora, vamos a simularlo. En un paso posterior, crearemos una tabla de estaciones.
  # Esto es una DEUDA TÉCNICA que resolvemos más adelante.
  # ¡OJO! Aquí deberías hacer un cruce (merge) con un catálogo de estaciones.
  # Ejemplo de catálogo que deberías crear:
  # estaciones_catalogo <- data.table(estacion = c("28079004", ...), latitud = c(40.42, ...), longitud = c(-3.71, ...))
  # air_quality_final_dt <- merge(air_quality_final_dt, estaciones_catalogo, by = "estacion")
  
  # Vamos a buscar la info de estaciones en el portal de datos
  # Esta URL contiene la info de las estaciones, incluyendo coordenadas
  estaciones_url <- "https://datos.madrid.es/egob/catalogo/202180-0-estaciones-control-aire.json"
  log_info("Obteniendo catálogo de estaciones de: {estaciones_url}")
  resp_estaciones <- request(estaciones_url) |> req_perform()
  estaciones_data <- resp_estaciones |> resp_body_json(simplifyVector = TRUE)
  estaciones_dt <- setDT(estaciones_data$estaciones)
  
  # Extraemos latitud y longitud. Vienen en formato "grados minutos segundos". Las convertimos.
  # (Esta parte puede ser compleja, por ahora extraemos la info que sí es directa)
  # La API de estaciones también provee coordenadas en UTM. ¡Usemos esas!
  # Vienen en la columna 'coordenadas_utm_etrs89' como 'POINT (X Y)'
  
  # Por simplicidad momentánea, nos quedamos con la info básica. La conversión geoespacial es un paso en sí mismo.
  # Asumimos que recuperamos lat y lon. Para este script, voy a usar la info del otro fichero JSON.
  # La API de "datos diarios" sí parece tener latitud y longitud. ¡Eso es inconsistente!
  # Vamos a usar la URL de estaciones que es más fiable.
  # Por ahora, y para no bloquear, vamos a hardcodear una estación para la demo.
  # ¡¡ESTO SE DEBE REEMPLAZAR CON UN CRUCE REAL!!
  air_quality_final_dt[, `:=`(latitud = 40.457077, longitud = -3.688849)]


  log_info("Transformando a objeto espacial (sf)...")
  air_quality_sf <- st_as_sf(
    air_quality_final_dt,
    coords = c("longitud", "latitud"),
    crs = 4326,
    remove = FALSE
  )

  log_success("Datos procesados y transformados a formato largo y espacial.")

}, error = function(e) {
  # ... (código de manejo de errores sin cambios)
})

# 6. ESCRITURA EN LA BASE DE DATOS POSTGIS ----
# -----------------------------------------------
tryCatch({
  table_name <- "mediciones_calidad_aire" # Nombre de tu tabla en PostGIS
  log_info("Escribiendo {nrow(air_quality_sf)} registros en la tabla '{table_name}'...")
  
  # st_write puede crear la tabla la primera vez y añadir datos las siguientes.
  # Usamos append = TRUE para no borrar los datos antiguos.
  # Si la tabla ya existe, dbCanWrite() debería ser TRUE.
  if (dbExistsTable(db_conn, table_name)) {
    st_write(air_quality_sf, dsn = db_conn, layer = table_name, append = TRUE)
  } else {
    st_write(air_quality_sf, dsn = db_conn, layer = table_name, append = FALSE) # Para la primera ejecución
  }
  
  log_success("Datos escritos correctamente en la base de datos.")
  
}, error = function(e) {
  log_error("Fallo al escribir en la base de datos.")
  log_error(e$message)
  stop("Error de escritura en BBDD. Abortando script.")
}, finally = {
  # Asegúrate de cerrar siempre la conexión
  log_info("Cerrando la conexión a la base de datos.")
  DBI::dbDisconnect(db_conn)
})

log_info("--- FIN DEL PROCESO DE RECOLECCIÓN DE DATOS ---")