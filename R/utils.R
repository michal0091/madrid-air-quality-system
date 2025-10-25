# FICHERO DE FUNCIONES AUXILIARES (UTILITIES)
# --------------------------------------------------------------------
# Contiene funciones reutilizables que pueden ser llamadas desde
# diferentes scripts del proyecto.
# --------------------------------------------------------------------


#' Procesa un lote de datos crudos de calidad del aire, los transforma y los carga en la BBDD
#'
#' @param datos_crudos Un data.table con los datos en formato ancho.
#' @param db_conn Una conexión DBI activa a la base de datos PostgreSQL.
#' @param dim_estaciones Un data.table con la tabla de dimensiones de las estaciones.
#'
#' @return NULL (invisible).
#' @export
procesar_y_cargar_lote_calidad_aire <- function(datos_crudos, db_conn, dim_estaciones) {
  log_info("Iniciando procesamiento de {nrow(datos_crudos)} filas crudas...")

  datos_largos <- melt(
    datos_crudos,
    id.vars = c(
      "PROVINCIA", "MUNICIPIO", "ESTACION", "MAGNITUD", 
      "PUNTO_MUESTREO", "ANO", "MES", "DIA"
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
  setnames(datos_largos, "ESTACION", "id_estacion_legacy") # Renombrar para evitar colisión

  # [FIX 1] Cruzar con dimensiones usando sintaxis de data.table join
  # (Equivalente a un left-join: dim_estaciones[datos_largos, ...])
  log_info("Enriqueciendo con dim_estaciones usando join de data.table...")
  datos_enriquecidos <- dim_estaciones[datos_largos, on = .(id_estacion == id_estacion_legacy)]

  if (any(is.na(datos_enriquecidos$codigo_largo))) {
    log_warn(
      "Se encontraron {sum(is.na(datos_enriquecidos$codigo_largo))} mediciones de estaciones no presentes en 'dim_estaciones'. Serán omitidas."
    )
    datos_enriquecidos <- datos_enriquecidos[!is.na(codigo_largo)]
  }

  fact_mediciones_dt <- datos_enriquecidos[, .(
    id_estacion, # Esta es la FK de dim_estaciones
    id_magnitud,
    fecha_hora,
    valor_medido = valor
  )]

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
  fecha_inicio_str <- format(fecha_inicio, "%Y-%m-%dT00:00:00UTC")
  fecha_fin_str <- format(fecha_fin, "%Y-%m-%dT23:59:59UTC")

  url_base <- glue(
    "https://opendata.aemet.es/opendata/api/valores/climatologicos/diarios/datos/fechaini/{fecha_inicio_str}/fechafin/{fecha_fin_str}/estacion/{estacion_id}"
  )

  for (intento in 1:max_intentos) {
    tryCatch(
      {
        log_info(
          "Intento {intento}/{max_intentos} para estación {estacion_id}, período {fecha_inicio} a {fecha_fin}"
        )

        resp_inicial <- request(url_base) |>
          req_url_query(api_key = api_key) |>
          req_timeout(30) |>
          req_user_agent("ProyectoCalidadAireMadrid/1.0") |>
          req_perform()

        if (resp_status(resp_inicial) != 200) {
          stop(glue("Error en respuesta inicial: {resp_status(resp_inicial)}"))
        }

        respuesta_json <- resp_body_json(resp_inicial)

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

        # --- Funciones auxiliares anidadas (robustas) ---
        as_numeric_data <- function(x) {
          if (is.null(x) || is.na(x) || x == "") {
            return(NA_real_) # Devolver NA numérico
          }
          as.numeric(gsub(",", ".", x))
        }

        as_POSIXct_data <- function(fecha, hora) {
          if (is.null(hora) || is.na(hora) || hora == "") {
            return(as.POSIXct(NA_character_)) # [FIX 4]
          }
          
          hora_limpia <- gsub(":", "", trimws(hora))

          if (hora_limpia == "24") {
            fecha_siguiente <- as.Date(fecha) + 1
            return(as.POSIXct(
              paste(fecha_siguiente, "00:00:00"),
              format = "%Y-%m-%d %H:%M:%S",
              tz = "Europe/Madrid"
            ))
          }
          
          if (nchar(hora_limpia) == 2) {
            hora_formateada <- paste0(hora_limpia, ":00")
          } else if (nchar(hora_limpia) == 4) {
            hora_formateada <- paste0(substr(hora_limpia, 1, 2), ":", substr(hora_limpia, 3, 4))
          } else if (grepl(":", hora)) {
            hora_formateada <- hora
          } else {
            return(as.POSIXct(NA_character_)) # [FIX 4]
          }
          
          fh <- paste(fecha, hora_formateada, sep = " ")
          as.POSIXct(fh, format = "%Y-%m-%d %H:%M", tz = "Europe/Madrid")
        }
        # --- Fin funciones anidadas ---
        datos_dt <- rbindlist(
          lapply(datos_json, function(x) {
            fecha_procesada <- as.Date(x$fecha)
            list(
              id_estacion_aemet = as.character(x$indicativo %||% estacion_id),
              fecha = fecha_procesada,
              nombre_estacion = as.character(x$nombre %||% NA),
              provincia = as.character(x$provincia %||% NA),
              altitud_m = as_numeric_data(x$altitud),
              temp_media_c = as_numeric_data(x$tmed),
              temp_maxima_c = as_numeric_data(x$tmax),
              temp_maxima_hora = as_POSIXct_data(fecha_procesada, x$horatmax),
              temp_minima_c = as_numeric_data(x$tmin),
              temp_minima_hora = as_POSIXct_data(fecha_procesada, x$horatmin),
              precipitacion_mm = as_numeric_data(x$prec),
              vel_viento_media_ms = as_numeric_data(x$velmedia),
              dir_viento_grados = as_numeric_data(x$dir),
              racha_maxima_ms = as_numeric_data(x$racha),
              hora_racha_maxima = as_POSIXct_data(fecha_procesada, x$horaracha),
              presion_maxima_hpa = as_numeric_data(x$presMax),
              hora_presion_maxima = as_POSIXct_data(fecha_procesada, x$horaPresMax),
              presion_minima_hpa = as_numeric_data(x$presMin),
              hora_presion_minima = as_POSIXct_data(fecha_procesada, x$horaPresMin),
              humedad_media_pct = as_numeric_data(x$hrMedia),
              humedad_maxima_pct = as_numeric_data(x$hrMax),
              hora_humedad_maxima = as_POSIXct_data(fecha_procesada, x$horaHrMax),
              humedad_minima_pct = as_numeric_data(x$hrMin),
              hora_humedad_minima = as_POSIXct_data(fecha_procesada, x$horaHrMin),
              ubicacion = paste(x$provincia %||% "", x$nombre %||% "", sep = "-") %||% NA
            )
          }),
          fill = TRUE
        )

        datos_dt <- datos_dt[!is.na(fecha)]

        log_success(
          "Descargados {nrow(datos_dt)} registros válidos para período {fecha_inicio} a {fecha_fin}"
        )
        return(datos_dt)
      },
      error = function(e) {
        log_error("Error en intento {intento}: {e$message}")
        if (intento < max_intentos) {
          tiempo_espera <- 2^intento 
          log_info("Esperando {tiempo_espera} segundos antes del siguiente intento...")
          Sys.sleep(tiempo_espera)
        }
      }
    )
  }

  log_error("Agotados todos los intentos para estación {estacion_id}")
  return(data.table())
}


# [FIX 6] Función 'to_db_value' eliminada por ser código muerto.

#' Carga bulk de datos meteorológicos diarios con manejo de duplicados (UPSERT)
#'
#' @param datos_meteo data.table con los datos a cargar
#' @param db_conn Conexión DBI
#' @param tabla_destino Nombre de la tabla de destino
#' @param manejar_duplicados TRUE para realizar un UPSERT (ON CONFLICT DO UPDATE)
#' @return Número de registros afectados
cargar_meteo_diarios_bulk <- function(
  datos_meteo, 
  db_conn, 
  tabla_destino = "fact_meteo_diaria", 
  manejar_duplicados = TRUE
) {
  
  if (nrow(datos_meteo) == 0) {
    log_warn("No hay datos para cargar")
    return(0)
  }
  
  log_info("Iniciando carga BULK de {nrow(datos_meteo)} registros meteorológicos diarios...")
  
  # Definición de columnas
  numeric_cols <- c(
    "altitud_m", "temp_media_c", "temp_maxima_c", "temp_minima_c",
    "precipitacion_mm", "vel_viento_media_ms", "dir_viento_grados",
    "racha_maxima_ms", "presion_maxima_hpa", "presion_minima_hpa",
    "humedad_media_pct", "humedad_maxima_pct", "humedad_minima_pct"
  )
  timestamp_cols <- c(
    "temp_maxima_hora", "temp_minima_hora", "hora_racha_maxima",
    "hora_presion_maxima", "hora_presion_minima", "hora_humedad_maxima",
    "hora_humedad_minima"
  )
  
  # Crear tabla (comando separado)
  crear_tabla_sql <- glue_sql("
  CREATE TABLE IF NOT EXISTS {`tabla_destino`} (
    id SERIAL PRIMARY KEY,
    id_estacion_aemet VARCHAR(10) NOT NULL,
    fecha DATE NOT NULL,
    nombre_estacion VARCHAR(255),
    provincia VARCHAR(100),
    altitud_m NUMERIC(6,1),
    temp_media_c NUMERIC(6,2),
    temp_maxima_c NUMERIC(6,2),
    temp_maxima_hora TIMESTAMP WITH TIME ZONE,
    temp_minima_c NUMERIC(6,2),
    temp_minima_hora TIMESTAMP WITH TIME ZONE,
    precipitacion_mm NUMERIC(8,2),
    vel_viento_media_ms NUMERIC(6,2),
    dir_viento_grados NUMERIC(5,1),
    racha_maxima_ms NUMERIC(6,2),
    hora_racha_maxima TIMESTAMP WITH TIME ZONE,
    presion_maxima_hpa NUMERIC(7,1),
    hora_presion_maxima TIMESTAMP WITH TIME ZONE,
    presion_minima_hpa NUMERIC(7,1),
    hora_presion_minima TIMESTAMP WITH TIME ZONE,
    humedad_media_pct NUMERIC(5,1),
    humedad_maxima_pct NUMERIC(5,1),
    hora_humedad_maxima TIMESTAMP WITH TIME ZONE,
    humedad_minima_pct NUMERIC(5,1),
    hora_humedad_minima TIMESTAMP WITH TIME ZONE,
    ubicacion VARCHAR(255),
    fecha_carga TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,
    CONSTRAINT unique_estacion_fecha UNIQUE(id_estacion_aemet, fecha)
  )", .con = db_conn)
  
  dbExecute(db_conn, crear_tabla_sql)
  
  # Crear índices
  indices_sql <- list(
    glue_sql("CREATE INDEX IF NOT EXISTS idx_meteo_diaria_estacion_fecha ON {`tabla_destino`} (id_estacion_aemet, fecha DESC)", .con = db_conn),
    glue_sql("CREATE INDEX IF NOT EXISTS idx_meteo_diaria_fecha ON {`tabla_destino`} (fecha DESC)", .con = db_conn)
  )
  lapply(indices_sql, function(sql) dbExecute(db_conn, sql)) # Usar lapply
  
  # Preparar los datos para inserción bulk
  datos_para_db <- copy(datos_meteo)
  datos_para_db[, fecha := as.Date(fecha)]
  
  log_info("Limpiando y preparando datos para inserción (método data.table)...")
  
  limpiar_valor <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA)
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) {
      x[x == "" | x == " " | tolower(x) == "null"] <- NA
    }
    return(x)
  }

  cols_all <- names(datos_para_db)
  datos_para_db[, (cols_all) := lapply(.SD, limpiar_valor), .SDcols = cols_all]
  
  numeric_cols_exist <- intersect(numeric_cols, cols_all)
  if (length(numeric_cols_exist) > 0) {
    datos_para_db[, (numeric_cols_exist) := lapply(.SD, as.numeric), .SDcols = numeric_cols_exist]
  }
  
  timestamp_cols_exist <- intersect(timestamp_cols, cols_all)
  if (length(timestamp_cols_exist) > 0) {
    datos_para_db[, (timestamp_cols_exist) := lapply(.SD, as.character), .SDcols = timestamp_cols_exist]
  }
  
  log_info("Escribiendo datos usando dbWriteTable...")
  
  if (manejar_duplicados) {
    tabla_temp <- paste0(tabla_destino, "_temp")
    dbWriteTable(db_conn, tabla_temp, datos_para_db, overwrite = TRUE, row.names = FALSE)
    
    # Definir columnas explícitamente para evitar errores
    # (Excluye 'id' y 'fecha_carga' que son manejadas por la BBDD)
    cols_db <- c(
      "id_estacion_aemet", "fecha", "nombre_estacion", "provincia", "altitud_m",
      "temp_media_c", "temp_maxima_c", "temp_maxima_hora", "temp_minima_c", "temp_minima_hora",
      "precipitacion_mm", "vel_viento_media_ms", "dir_viento_grados", "racha_maxima_ms", "hora_racha_maxima",
      "presion_maxima_hpa", "hora_presion_maxima", "presion_minima_hpa", "hora_presion_minima",
      "humedad_media_pct", "humedad_maxima_pct", "hora_humedad_maxima", "humedad_minima_pct", "hora_humedad_minima",
      "ubicacion"
    )
    
    # Construir la parte del 'SELECT' con casting explícito
    select_casts <- sapply(cols_db, function(col) {
      if (col == "fecha") {
        "fecha::date"
      } else if (col %in% numeric_cols) {
        glue("CASE WHEN {col} IS NULL OR TRIM({col}::text) = '' THEN NULL ELSE {col}::numeric END AS {col}")
      } else if (col %in% timestamp_cols) {
        glue("CASE WHEN {col} IS NULL OR TRIM({col}::text) = '' THEN NULL ELSE {col}::timestamp with time zone END AS {col}")
      } else {
        col # Columnas de texto
      }
    })
    
    # Construir la parte 'DO UPDATE'
    update_set <- paste(
      glue("{cols_db} = EXCLUDED.{cols_db}"),
      collapse = ",\n    "
    )
    
    upsert_sql <- glue_sql('
    INSERT INTO {`tabla_destino`} (
      {DBI::SQL(paste(cols_db, collapse = ",\n      "))}
    )
    SELECT 
      {DBI::SQL(paste(select_casts, collapse = ",\n      "))}
    FROM {`tabla_temp`}
    ON CONFLICT (id_estacion_aemet, fecha) 
    DO UPDATE SET
      {DBI::SQL(update_set)},
      fecha_carga = CURRENT_TIMESTAMP
    ', .con = db_conn)
    
    registros_insertados <- dbExecute(db_conn, upsert_sql)
    dbExecute(db_conn, glue_sql("DROP TABLE {`tabla_temp`}", .con = db_conn))
    
  } else {
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