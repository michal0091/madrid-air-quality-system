# FICHERO DE FUNCIONES AUXILIARES (UTILITIES)
# --------------------------------------------------------------------
# Contiene funciones reutilizables que pueden ser llamadas desde
# diferentes scripts del proyecto.
# --------------------------------------------------------------------

#' Procesa un lote de datos crudos de calidad del aire, los transforma y los carga en la BBDD
#'
#' Esta función toma datos de calidad del aire en formato ancho (con columnas H01-H24 para valores
#' y V01-V24 para validación), los transforma a formato largo, filtra por validez,
#' enriquece con dimensiones de estaciones y carga los resultados en la tabla de hechos.
#'
#' @param datos_crudos Un data.table con los datos en formato ancho, leídos desde un CSV.
#'   Debe contener las columnas: PROVINCIA, MUNICIPIO, ESTACION, MAGNITUD, PUNTO_MUESTREO,
#'   ANO, MES, DIA, y columnas H01-H24 (valores) y V01-V24 (validación).
#' @param db_conn Una conexión DBI activa a la base de datos PostgreSQL donde se cargarán los datos.
#' @param dim_estaciones Un data.table con la tabla de dimensiones de las estaciones.
#'   Debe contener al menos las columnas: id_estacion, codigo_largo.
#'
#' @details
#' La función realiza las siguientes operaciones:
#' \itemize{
#'   \item Transforma los datos de formato ancho a largo usando data.table::melt
#'   \item Filtra solo las mediciones marcadas como válidas (V)
#'   \item Convierte los valores a numérico, manejando comas decimales
#'   \item Crea timestamps con zona horaria Europe/Madrid
#'   \item Cruza con dimensiones de estaciones para obtener claves primarias
#'   \item Carga los datos procesados en la tabla 'fact_mediciones'
#' }
#'
#' @return NULL (invisible). La función escribe en la BBDD como efecto secundario.
#'   En caso de error o datos vacíos, retorna NULL sin cargar datos.
#'
#' @examples
#' \dontrun{
#'   # Ejemplo de uso típico
#'   datos <- fread("datos_raw.csv")
#'   conn <- dbConnect(RPostgres::Postgres(), ...)
#'   dim_est <- dbReadTable(conn, "dim_estaciones")
#'   procesar_y_cargar_lote(datos, conn, dim_est)
#' }
#'
#' @seealso \code{\link[data.table]{melt}} para la transformación de formato,
#'   \code{\link[DBI]{dbWriteTable}} para la carga en base de datos
#'
#' @export
procesar_y_cargar_lote <- function(datos_crudos, db_conn, dim_estaciones) {
  log_info("Iniciando procesamiento de {nrow(datos_crudos)} filas crudas...")

  # Lógica de transformación de ancho a largo
  datos_largos <- melt(
    datos_crudos,
    id.vars = c(
      "PROVINCIA",
      "MUNICIPIO",
      "ESTACION",
      "MAGNITUD",
      "PUNTO_MUESTREO",
      "ANO",
      "MES",
      "DIA"
    ),
    measure.vars = patterns("^H", "^V"),
    variable.name = "hora_indice",
    value.name = c("valor", "valido")
  )

  datos_largos <- datos_largos[valido == "V"]
  if (nrow(datos_largos) == 0) {
    log_warn("El lote no contenía datos con validación 'V'. Omitiendo.")
    return(invisible(NULL))
  }
  datos_largos[, hora := as.integer(hora_indice) - 1]
  datos_largos[, `:=`(
    valor = as.numeric(sub(",", ".", valor, fixed = TRUE)),
    fecha_hora = make_datetime(ANO, MES, DIA, hora, tz = "Europe/Madrid")
  )]

  setnames(datos_largos, "MAGNITUD", "id_magnitud")
  setnames(datos_largos, "ESTACION", "id_estacion")

  # Cruzar con las dimensiones para obtener la clave primaria de las estaciones
  datos_enriquecidos <- merge(
    datos_largos,
    dim_estaciones[, .(codigo_largo, id_estacion)],
    by = "id_estacion",
    all.x = TRUE
  ) # Usamos all.x=TRUE para no perder filas si una estación no se encuentra

  if (any(is.na(datos_enriquecidos$id_estacion))) {
    log_warn(
      "Se encontraron {sum(is.na(datos_enriquecidos$id_estacion))} mediciones de estaciones no presentes en 'dim_estaciones'. Serán omitidas."
    )
    datos_enriquecidos <- na.omit(datos_enriquecidos, cols = "id_estacion")
  }

  # Crear la tabla final de hechos, con las columnas exactas para la BBDD
  fact_mediciones_dt <- datos_enriquecidos[, .(
    id_estacion,
    id_magnitud,
    fecha_hora,
    valor_medido = valor
  )]

  # Escribir la tabla de hechos en la base de datos
  dbWriteTable(db_conn, "fact_mediciones", fact_mediciones_dt, append = TRUE)
  log_success(
    "Cargados {nrow(fact_mediciones_dt)} registros en 'fact_mediciones'."
  )

  return(invisible(NULL))
}


#' Obtiene datos climatológicos diarios de AEMET para un período específico
#'
#' @param api_key Clave de API de AEMET
#' @param estacion_id ID de la estación meteorológica
#' @param fecha_inicio Fecha de inicio (Date)
#' @param fecha_fin Fecha de fin (Date)
#' @param max_intentos Número máximo de reintentos en caso de fallo
#' @return data.table con los datos meteorológicos diarios
obtener_datos_aemet_periodo <- function(
  api_key,
  estacion_id,
  fecha_inicio,
  fecha_fin,
  max_intentos = 3
) {
  # Convertir fechas al formato requerido por AEMET (YYYY-MM-DDTHH:MM:SSUTC)
  fecha_inicio_str <- format(fecha_inicio, "%Y-%m-%dT00:00:00UTC")
  fecha_fin_str <- format(fecha_fin, "%Y-%m-%dT23:59:59UTC")

  # URL para datos climatológicos diarios
  url_base <- glue(
    "https://opendata.aemet.es/opendata/api/valores/climatologicos/diarios/datos/fechaini/{fecha_inicio_str}/fechafin/{fecha_fin_str}/estacion/{estacion_id}"
  )

  for (intento in 1:max_intentos) {
    tryCatch(
      {
        log_info(
          "Intento {intento}/{max_intentos} para estación {estacion_id}, período {fecha_inicio} a {fecha_fin}"
        )

        # Primera petición para obtener la URL de descarga
        resp_inicial <- request(url_base) |>
          req_url_query(api_key = api_key) |>
          req_timeout(30) |>
          req_user_agent("ProyectoCalidadAireMadrid/1.0") |>
          req_perform()

        if (resp_status(resp_inicial) != 200) {
          stop(glue("Error en respuesta inicial: {resp_status(resp_inicial)}"))
        }

        respuesta_json <- resp_body_json(resp_inicial)

        # Verificar estado de la respuesta
        if (respuesta_json$estado != 200) {
          if (respuesta_json$estado == 404) {
            log_warn("Sin datos disponibles para el período solicitado")
            return(data.table())
          }
          stop(glue(
            "Error en API de AEMET: {respuesta_json$descripcion} (Estado: {respuesta_json$estado})"
          ))
        }

        url_datos <- respuesta_json$datos

        # Segunda petición para obtener los datos reales
        Sys.sleep(2) # Pausa entre peticiones

        resp_datos <- request(url_datos) |>
          req_timeout(60) |>
          req_user_agent("ProyectoCalidadAireMadrid/1.0") |>
          req_perform()

        if (resp_status(resp_datos) != 200) {
          stop(glue("Error al descargar datos: {resp_status(resp_datos)}"))
        }

        datos_json <- resp_body_string(resp_datos)
        datos_json <- jsonlite::parse_json(datos_json)

        if (length(datos_json) == 0) {
          log_warn("Sin datos para el período solicitado")
          return(data.table())
        }
        # Funciones auxiliares corregidas para procesar datos de AEMET

        as_numeric_data <- function(x) {
          if (is.null(x) || is.na(x) || x == "") {
            return(NA)
          }
          as.numeric(gsub(",", ".", x))
        }

        # Función corregida para manejar las horas de AEMET
        as_POSIXct_data <- function(fecha, hora) {
          if (is.null(hora) || is.na(hora) || hora == "") {
            return(NA)
          }

          # Limpiar la hora y manejar casos especiales
          hora_limpia <- gsub(":", "", trimws(hora))

          # Si la hora es "24", convertir a "00" del día siguiente
          if (hora_limpia == "24") {
            fecha_siguiente <- as.Date(fecha) + 1
            return(as.POSIXct(
              paste(fecha_siguiente, "00:00:00"),
              format = "%Y-%m-%d %H:%M:%S",
              tz = "Europe/Madrid"
            ))
          }

          # Para horas de 2 dígitos (ej: "00", "12"), asumir minutos 00
          if (nchar(hora_limpia) == 2) {
            hora_formateada <- paste0(hora_limpia, ":00")
          } else if (nchar(hora_limpia) == 4) {
            # Para formato HHMM (ej: "1240")
            hora_formateada <- paste0(
              substr(hora_limpia, 1, 2),
              ":",
              substr(hora_limpia, 3, 4)
            )
          } else if (grepl(":", hora)) {
            # Ya tiene formato HH:MM
            hora_formateada <- hora
          } else {
            return(NA)
          }

          fh <- paste(fecha, hora_formateada, sep = " ")
          as.POSIXct(fh, format = "%Y-%m-%d %H:%M", tz = "Europe/Madrid")
        }

        # Convertir JSON a data.table usando la estructura real de AEMET
        datos_dt <- rbindlist(
          map(datos_json, function(x) {
            fecha_procesada <- as.Date(x$fecha)

            list(
              id_estacion_aemet = as.character(x$indicativo %||% estacion_id),
              fecha = fecha_procesada,
              nombre_estacion = as.character(x$nombre %||% NA),
              provincia = as.character(x$provincia %||% NA),
              altitud_m = as_numeric_data(x$altitud),

              # Temperaturas
              temp_media_c = as_numeric_data(x$tmed),
              temp_maxima_c = as_numeric_data(x$tmax),
              temp_maxima_hora = as_POSIXct_data(fecha_procesada, x$horatmax),
              temp_minima_c = as_numeric_data(x$tmin),
              temp_minima_hora = as_POSIXct_data(fecha_procesada, x$horatmin),

              # Precipitación
              precipitacion_mm = as_numeric_data(x$prec),

              # Viento
              vel_viento_media_ms = as_numeric_data(x$velmedia),
              dir_viento_grados = as_numeric_data(x$dir),
              racha_maxima_ms = as_numeric_data(x$racha),
              hora_racha_maxima = as_POSIXct_data(fecha_procesada, x$horaracha),

              # Presión atmosférica
              presion_maxima_hpa = as_numeric_data(x$presMax),
              hora_presion_maxima = as_POSIXct_data(
                fecha_procesada,
                x$horaPresMax
              ),
              presion_minima_hpa = as_numeric_data(x$presMin),
              hora_presion_minima = as_POSIXct_data(
                fecha_procesada,
                x$horaPresMin
              ),

              # Humedad relativa
              humedad_media_pct = as_numeric_data(x$hrMedia),
              humedad_maxima_pct = as_numeric_data(x$hrMax),
              hora_humedad_maxima = as_POSIXct_data(
                fecha_procesada,
                x$horaHrMax
              ),
              humedad_minima_pct = as_numeric_data(x$hrMin),
              hora_humedad_minima = as_POSIXct_data(
                fecha_procesada,
                x$horaHrMin
              ),

              # Ubicación (construir desde nombre y provincia)
              ubicacion = paste(
                x$provincia %||% "",
                x$nombre %||% "",
                sep = "-"
              ) %||%
                NA
            )
          }),
          fill = TRUE
        )

        # Filtrar registros válidos
        datos_dt <- datos_dt[!is.na(fecha)]

        log_success(
          "Descargados {nrow(datos_dt)} registros válidos para período {fecha_inicio} a {fecha_fin}"
        )
        return(datos_dt)
      },
      error = function(e) {
        log_error("Error en intento {intento}: {e$message}")

        if (intento < max_intentos) {
          tiempo_espera <- 2^intento # Backoff exponencial
          log_info(
            "Esperando {tiempo_espera} segundos antes del siguiente intento..."
          )
          Sys.sleep(tiempo_espera)
        }
      }
    )
  }

  log_error("Agotados todos los intentos para estación {estacion_id}")
  return(data.table())
}


# Función para convertir valores a formato de base de datos
to_db_value <- function(val) {
  # Verificar si es NULL, NA, o vacío
  if (is.null(val) || length(val) == 0) {
    return(NA)  # PostgreSQL entiende NA como NULL
  }
  
  # Si es un vector de longitud > 1, tomar solo el primer elemento
  if (length(val) > 1) {
    val <- val[1]
  }
  
  # Verificar si es NA después de tomar el primer elemento
  if (is.na(val)) {
    return(NA)
  }
  
  # Manejo especial para fechas/timestamps
  if (inherits(val, "POSIXct")) {
    if (is.na(val)) {
      return(NA)
    }
    # Convertir a string en formato ISO para PostgreSQL
    return(as.character(val))
  }
  
  # Para otros tipos, devolver el valor tal como está
  return(val)
}

# Función alternativa más robusta usando dbWriteTable en lotes
cargar_meteo_diarios_bulk <- function(datos_meteo, db_conn, tabla_destino = "fact_meteo_diaria", manejar_duplicados = TRUE) {
  
  if (nrow(datos_meteo) == 0) {
    log_warn("No hay datos para cargar")
    return(0)
  }
  
  log_info("Iniciando carga BULK de {nrow(datos_meteo)} registros meteorológicos diarios...")
  
  # Crear tabla (comando separado)
  crear_tabla_sql <- glue_sql("
  CREATE TABLE IF NOT EXISTS {`tabla_destino`} (
    id SERIAL PRIMARY KEY,
    id_estacion_aemet VARCHAR(10) NOT NULL,
    fecha DATE NOT NULL,
    nombre_estacion VARCHAR(255),
    provincia VARCHAR(100),
    altitud_m NUMERIC(6,1),
    
    -- Temperaturas
    temp_media_c NUMERIC(6,2),
    temp_maxima_c NUMERIC(6,2),
    temp_maxima_hora TIMESTAMP WITH TIME ZONE,
    temp_minima_c NUMERIC(6,2),
    temp_minima_hora TIMESTAMP WITH TIME ZONE,
    
    -- Precipitación
    precipitacion_mm NUMERIC(8,2),
    
    -- Viento
    vel_viento_media_ms NUMERIC(6,2),
    dir_viento_grados NUMERIC(5,1),
    racha_maxima_ms NUMERIC(6,2),
    hora_racha_maxima TIMESTAMP WITH TIME ZONE,
    
    -- Presión atmosférica
    presion_maxima_hpa NUMERIC(7,1),
    hora_presion_maxima TIMESTAMP WITH TIME ZONE,
    presion_minima_hpa NUMERIC(7,1),
    hora_presion_minima TIMESTAMP WITH TIME ZONE,
    
    -- Humedad relativa
    humedad_media_pct NUMERIC(5,1),
    humedad_maxima_pct NUMERIC(5,1),
    hora_humedad_maxima TIMESTAMP WITH TIME ZONE,
    humedad_minima_pct NUMERIC(5,1),
    hora_humedad_minima TIMESTAMP WITH TIME ZONE,
    
    -- Metadatos
    ubicacion VARCHAR(255),
    fecha_carga TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    
    CONSTRAINT unique_estacion_fecha UNIQUE(id_estacion_aemet, fecha)
  )", .con = db_conn)
  
  # Ejecutar creación de tabla
  dbExecute(db_conn, crear_tabla_sql)
  
  # Crear índices (comandos separados)
  indices_sql <- list(
    glue_sql("CREATE INDEX IF NOT EXISTS idx_meteo_diaria_estacion_fecha 
             ON {`tabla_destino`} (id_estacion_aemet, fecha DESC)", .con = db_conn),
    glue_sql("CREATE INDEX IF NOT EXISTS idx_meteo_diaria_fecha 
             ON {`tabla_destino`} (fecha DESC)", .con = db_conn),
    glue_sql("CREATE INDEX IF NOT EXISTS idx_meteo_diaria_provincia 
             ON {`tabla_destino`} (provincia)", .con = db_conn)
  )
  
  # Ejecutar cada índice por separado
  for (sql_indice in indices_sql) {
    dbExecute(db_conn, sql_indice)
  }
  
  # Preparar los datos para inserción bulk
  datos_para_db <- copy(datos_meteo)
  
  # Limpiar datos problemáticos
  datos_para_db[, fecha := as.Date(fecha)]
  
  # Limpiar y preparar datos para PostgreSQL
  log_info("Limpiando y preparando datos para inserción...")
  
  # Función para limpiar valores problemáticos
  limpiar_valor <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA)
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) {
      x[x == "" | x == " " | tolower(x) == "null"] <- NA
    }
    return(x)
  }
  
  # Limpiar todas las columnas
  for (col in names(datos_para_db)) {
    datos_para_db[, (col) := limpiar_valor(get(col))]
  }
  
  # Convertir timestamps a character para PostgreSQL
  timestamp_cols <- c("temp_maxima_hora", "temp_minima_hora", "hora_racha_maxima", 
                     "hora_presion_maxima", "hora_presion_minima", "hora_humedad_maxima", 
                     "hora_humedad_minima")
  
  for (col in timestamp_cols) {
    if (col %in% names(datos_para_db)) {
      datos_para_db[, (col) := lapply(.SD, function(x) {
        if (inherits(x, "POSIXct")) {
          ifelse(is.na(x), NA_character_, as.character(x))
        } else if (is.character(x)) {
          ifelse(is.na(x) | x == "", NA_character_, x)
        } else {
          NA_character_
        }
      }), .SDcols = col]
    }
  }
  
  # Usar dbWriteTable para carga bulk más eficiente
  log_info("Escribiendo datos usando dbWriteTable...")
  
  if (manejar_duplicados) {
    # Para manejar duplicados, usar tabla temporal
    tabla_temp <- paste0(tabla_destino, "_temp")
    
    # Escribir a tabla temporal (sin la columna id auto-generada)
    dbWriteTable(db_conn, tabla_temp, datos_para_db, overwrite = TRUE, row.names = FALSE)
    
    # Hacer UPSERT desde tabla temporal especificando columnas explícitamente
    upsert_sql <- glue_sql("
    INSERT INTO {`tabla_destino`} 
    (id_estacion_aemet, fecha, nombre_estacion, provincia, altitud_m,
     temp_media_c, temp_maxima_c, temp_maxima_hora, temp_minima_c, temp_minima_hora,
     precipitacion_mm, vel_viento_media_ms, dir_viento_grados, 
     racha_maxima_ms, hora_racha_maxima,
     presion_maxima_hpa, hora_presion_maxima, presion_minima_hpa, hora_presion_minima,
     humedad_media_pct, humedad_maxima_pct, hora_humedad_maxima, 
     humedad_minima_pct, hora_humedad_minima, ubicacion)
    SELECT 
      id_estacion_aemet, 
      fecha::date, 
      nombre_estacion, 
      provincia, 
      CASE WHEN altitud_m IS NULL OR TRIM(altitud_m::text) = '' THEN NULL ELSE altitud_m::numeric END,
      CASE WHEN temp_media_c IS NULL OR TRIM(temp_media_c::text) = '' THEN NULL ELSE temp_media_c::numeric END,
      CASE WHEN temp_maxima_c IS NULL OR TRIM(temp_maxima_c::text) = '' THEN NULL ELSE temp_maxima_c::numeric END,
      CASE WHEN temp_maxima_hora IS NULL OR TRIM(temp_maxima_hora::text) = '' THEN NULL ELSE temp_maxima_hora::timestamp with time zone END,
      CASE WHEN temp_minima_c IS NULL OR TRIM(temp_minima_c::text) = '' THEN NULL ELSE temp_minima_c::numeric END,
      CASE WHEN temp_minima_hora IS NULL OR TRIM(temp_minima_hora::text) = '' THEN NULL ELSE temp_minima_hora::timestamp with time zone END,
      CASE WHEN precipitacion_mm IS NULL OR TRIM(precipitacion_mm::text) = '' THEN NULL ELSE precipitacion_mm::numeric END,
      CASE WHEN vel_viento_media_ms IS NULL OR TRIM(vel_viento_media_ms::text) = '' THEN NULL ELSE vel_viento_media_ms::numeric END,
      CASE WHEN dir_viento_grados IS NULL OR TRIM(dir_viento_grados::text) = '' THEN NULL ELSE dir_viento_grados::numeric END,
      CASE WHEN racha_maxima_ms IS NULL OR TRIM(racha_maxima_ms::text) = '' THEN NULL ELSE racha_maxima_ms::numeric END,
      CASE WHEN hora_racha_maxima IS NULL OR TRIM(hora_racha_maxima::text) = '' THEN NULL ELSE hora_racha_maxima::timestamp with time zone END,
      CASE WHEN presion_maxima_hpa IS NULL OR TRIM(presion_maxima_hpa::text) = '' THEN NULL ELSE presion_maxima_hpa::numeric END,
      CASE WHEN hora_presion_maxima IS NULL OR TRIM(hora_presion_maxima::text) = '' THEN NULL ELSE hora_presion_maxima::timestamp with time zone END,
      CASE WHEN presion_minima_hpa IS NULL OR TRIM(presion_minima_hpa::text) = '' THEN NULL ELSE presion_minima_hpa::numeric END,
      CASE WHEN hora_presion_minima IS NULL OR TRIM(hora_presion_minima::text) = '' THEN NULL ELSE hora_presion_minima::timestamp with time zone END,
      CASE WHEN humedad_media_pct IS NULL OR TRIM(humedad_media_pct::text) = '' THEN NULL ELSE humedad_media_pct::numeric END,
      CASE WHEN humedad_maxima_pct IS NULL OR TRIM(humedad_maxima_pct::text) = '' THEN NULL ELSE humedad_maxima_pct::numeric END,
      CASE WHEN hora_humedad_maxima IS NULL OR TRIM(hora_humedad_maxima::text) = '' THEN NULL ELSE hora_humedad_maxima::timestamp with time zone END,
      CASE WHEN humedad_minima_pct IS NULL OR TRIM(humedad_minima_pct::text) = '' THEN NULL ELSE humedad_minima_pct::numeric END,
      CASE WHEN hora_humedad_minima IS NULL OR TRIM(hora_humedad_minima::text) = '' THEN NULL ELSE hora_humedad_minima::timestamp with time zone END,
      ubicacion
    FROM {`tabla_temp`}
    ON CONFLICT (id_estacion_aemet, fecha) 
    DO UPDATE SET
      nombre_estacion = EXCLUDED.nombre_estacion,
      provincia = EXCLUDED.provincia,
      altitud_m = EXCLUDED.altitud_m,
      temp_media_c = EXCLUDED.temp_media_c,
      temp_maxima_c = EXCLUDED.temp_maxima_c,
      temp_maxima_hora = EXCLUDED.temp_maxima_hora,
      temp_minima_c = EXCLUDED.temp_minima_c,
      temp_minima_hora = EXCLUDED.temp_minima_hora,
      precipitacion_mm = EXCLUDED.precipitacion_mm,
      vel_viento_media_ms = EXCLUDED.vel_viento_media_ms,
      dir_viento_grados = EXCLUDED.dir_viento_grados,
      racha_maxima_ms = EXCLUDED.racha_maxima_ms,
      hora_racha_maxima = EXCLUDED.hora_racha_maxima,
      presion_maxima_hpa = EXCLUDED.presion_maxima_hpa,
      hora_presion_maxima = EXCLUDED.hora_presion_maxima,
      presion_minima_hpa = EXCLUDED.presion_minima_hpa,
      hora_presion_minima = EXCLUDED.hora_presion_minima,
      humedad_media_pct = EXCLUDED.humedad_media_pct,
      humedad_maxima_pct = EXCLUDED.humedad_maxima_pct,
      hora_humedad_maxima = EXCLUDED.hora_humedad_maxima,
      humedad_minima_pct = EXCLUDED.humedad_minima_pct,
      hora_humedad_minima = EXCLUDED.hora_humedad_minima,
      ubicacion = EXCLUDED.ubicacion,
      fecha_carga = CURRENT_TIMESTAMP
    ", .con = db_conn)
    
    registros_insertados <- dbExecute(db_conn, upsert_sql)
    
    # Limpiar tabla temporal
    dbExecute(db_conn, glue_sql("DROP TABLE {`tabla_temp`}", .con = db_conn))
    
  } else {
    # Inserción directa sin manejo de duplicados
    dbWriteTable(db_conn, tabla_destino, datos_para_db, append = TRUE, row.names = FALSE)
    registros_insertados <- nrow(datos_para_db)
  }
  
  log_info("Carga BULK completada. Registros insertados/actualizados: {registros_insertados}")
  return(registros_insertados)
}

#' Operador para valores por defecto (equivalente a ?? en otros lenguajes)
`%||%` <- function(x, y) {
  if (is.null(x) | length(x) == 0 | all(is.na(x))) y else x
}