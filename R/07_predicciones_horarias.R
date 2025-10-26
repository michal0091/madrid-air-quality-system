# SCRIPT 07: PREDICCIONES HORARIAS CON XGBOOST NATIVO
# --------------------------------------------------------------------
# Objetivo: Generar predicciones horarias (40h) usando modelos xgboost nativos
# y datos meteorológicos de AEMET (o fallback).
# Estilo: data.table puro + xgboost nativo
# --------------------------------------------------------------------

# 1. LIBRERÍAS ----
# library(sf) # Cargado condicionalmente si es necesario para UTM
library(data.table)
library(xgboost)     
library(lubridate)
library(logger)
library(glue)
library(DBI)          
library(RPostgres)    

# 2. CONFIGURACIÓN ----
log_appender(appender_tee("logs/predicciones_horarias_xgb_nativo.log")) # Nuevo log
log_info("--- INICIO PREDICCIONES HORARIAS (XGBOOST NATIVO) ---")
MODELOS_DIR <- "models" # Directorio donde están los .model
N_HORAS_PREDICCION <- 40

# 3. FUNCIONES AUXILIARES ----

# --- Generación/Obtención de Meteo (Convertido a data.table) ---
generar_meteo_futuro <- function(horas_futuras = N_HORAS_PREDICCION) {
  log_info("Generando datos meteorológicos de FALLBACK para {horas_futuras} horas...")
  inicio <- Sys.time(); inicio_hora <- floor_date(inicio, "hour")
  horas_seq <- seq(inicio_hora, by = "hour", length.out = horas_futuras)

  # Crear data.table directamente
  meteo_dt <- data.table(fecha_hora = horas_seq)
  meteo_dt[, `:=`(
      fecha = as.Date(fecha_hora),
      hora = hour(fecha_hora),
      dia_año = yday(fecha_hora),
      mes = month(fecha_hora)
  )]
  meteo_dt[, temp_base := fcase(
      mes %in% c(12, 1, 2), 8,
      mes %in% c(3, 4, 5), 18,
      mes %in% c(6, 7, 8), 28,
      mes %in% c(9, 10, 11), 18,
      default = 15 # Fallback
  )]
  meteo_dt[, `:=`(
      temp_diurna = sin((hora - 6) * pi / 12) * 6,
      temp_media_c = temp_base + sin((hora - 6) * pi / 12) * 6 + rnorm(.N, 0, 1.5)
  )]
  meteo_dt[, `:=`(
      humedad_media_pct = 70 - (temp_media_c - 20) * 1.5 + rnorm(.N, 0, 5),
      precipitacion_mm = fifelse(runif(.N) < 0.1, rexp(.N, 2), 0),
      vel_viento_media_ms = pmax(0, 2 + sin((hora - 12) * pi / 12) * 1.5 + abs(rnorm(.N, 0, 0.5))),
      dir_viento_grados = (200 + sin(hora * pi / 12) * 30 + rnorm(.N, 0, 20)) %% 360,
      presion_maxima_hpa = 1013 + sin((dia_año - 50) * 2 * pi / 365) * 8 + rnorm(.N, 0, 3)
  )]
  # Límites
  meteo_dt[, `:=`(
      temp_media_c = fcase(temp_media_c < 0, 0, temp_media_c > 45, 45, default = temp_media_c),
      humedad_media_pct = fcase(humedad_media_pct < 20, 20, humedad_media_pct > 100, 100, default = humedad_media_pct),
      presion_maxima_hpa = fcase(presion_maxima_hpa < 980, 980, presion_maxima_hpa > 1040, 1040, default = presion_maxima_hpa)
  )]

  log_info("Datos meteorológicos (fallback) generados: {nrow(meteo_dt)} horas")
  log_info("Rango temperaturas: {round(meteo_dt[, min(temp_media_c)], 1)}°C - {round(meteo_dt[, max(temp_media_c)], 1)}°C")
  return(meteo_dt)
}

obtener_meteo_aemet <- function(horas_futuras = N_HORAS_PREDICCION) {
  log_info("Intentando obtener predicciones AEMET para próximas {horas_futuras} horas...")
  archivo_aemet <- "data/realtime/prediccion_meteo_latest.rds"

  tryCatch({
    if (file.exists(archivo_aemet)) {
      datos_aemet_raw <- readRDS(archivo_aemet)
      setDT(datos_aemet_raw) 
      log_info("✅ Datos AEMET cargados desde archivo: {nrow(datos_aemet_raw)} registros")

      hora_actual_madrid <- floor_date(with_tz(Sys.time(), "Europe/Madrid"), "hour")
      proxima_hora_madrid <- hora_actual_madrid + hours(1)

      # Procesar con data.table
      datos_aemet_dt <- datos_aemet_raw[timestamp >= proxima_hora_madrid][order(timestamp)][1:min(.N, horas_futuras)]
      datos_aemet_dt[, `:=`(
          fecha_hora = timestamp,
          fecha = as.Date(timestamp),
          hora = hour(timestamp),
          dia_año = yday(timestamp),
          # Mapear columnas AEMET
          humedad_media_pct = humedad_relativa_pct,
          presion_maxima_hpa = presion_hpa, # Usar presion_hpa como proxy
          vel_viento_media_ms = velocidad_viento_ms
          # temp_media_c y dir_viento_grados deberían existir
      )]

      # Seleccionar columnas necesarias + fecha_hora original
      cols_necesarias_meteo <- c("fecha_hora", "fecha", "hora", "dia_año", "temp_media_c",
                                 "humedad_media_pct", "precipitacion_mm", "vel_viento_media_ms",
                                 "dir_viento_grados", "presion_maxima_hpa") # Añadir las que se usen en F.E.

      # Asegurarse de que las columnas existan, rellenar con NA si no
      for(col in cols_necesarias_meteo){
          if(!col %in% names(datos_aemet_dt)){
              log_warn("Columna AEMET faltante: '{col}'. Se rellenará con NA.")
              datos_aemet_dt[, (col) := NA]
          }
      }
      # Convertir tipos por si acaso
      datos_aemet_dt[, temp_media_c := as.numeric(temp_media_c)]
      datos_aemet_dt[, humedad_media_pct := as.numeric(humedad_media_pct)]
      datos_aemet_dt[, precipitacion_mm := fifelse(exists("precipitacion_mm"), as.numeric(precipitacion_mm), 0)] # Asumir 0 si no existe
      datos_aemet_dt[, vel_viento_media_ms := as.numeric(vel_viento_media_ms)]
      datos_aemet_dt[, dir_viento_grados := as.numeric(dir_viento_grados)]
      datos_aemet_dt[, presion_maxima_hpa := as.numeric(presion_maxima_hpa)]

      datos_aemet_dt <- datos_aemet_dt[, ..cols_necesarias_meteo]


      log_info("📅 Predicciones AEMET desde: {min(datos_aemet_dt$fecha_hora)} a {max(datos_aemet_dt$fecha_hora)}")
      log_info("⏰ Hora actual Madrid: {format(hora_actual_madrid)} -> Próxima hora: {format(proxima_hora_madrid)}")

      if (nrow(datos_aemet_dt) >= horas_futuras * 0.8) {
        log_success("✅ Usando {nrow(datos_aemet_dt)} predicciones AEMET reales.")
        return(datos_aemet_dt)
      } else {
         log_warn("⚠️ Pocos datos AEMET ({nrow(datos_aemet_dt)}), usando fallback.")
      }
    } else {
      log_warn("⚠️ Archivo AEMET '{archivo_aemet}' no encontrado, usando fallback.")
    }
  }, error = function(e) {
    log_error("❌ Error procesando AEMET: {e$message}")
    log_warn("⚠️ Usando fallback.")
  })

  return(generar_meteo_futuro(horas_futuras))
}


# --- Carga de Datos Auxiliares (Convertido a data.table) ---
cargar_datos_auxiliares <- function() {
  log_info("Cargando datos auxiliares (baseline)...")
  if (file.exists('.Renviron')) readRenviron('.Renviron')

  db_host <- Sys.getenv("DB_HOST"); db_port <- as.integer(Sys.getenv("DB_PORT"))
  db_name <- Sys.getenv("DB_NAME"); db_user <- Sys.getenv("DB_USER")
  db_password <- Sys.getenv("DB_PASSWORD")

  con_aux <- NULL
  tryCatch({
    log_info("🔌 Conectando a BD para datos auxiliares...")
    con_aux <- dbConnect(RPostgres::Postgres(), host=db_host, port=db_port, dbname=db_name, user=db_user, password=db_password)

    baseline_dt <- setDT(dbGetQuery(con_aux, "
      SELECT id_magnitud, mes, dia_mes, hora, promedio_5y, p10, p90
      FROM dim_baseline_estacional
    "))
    setkeyv(baseline_dt, c("id_magnitud", "mes", "dia_mes", "hora")) # Clave para join rápido
    log_success("✅ Baseline estacional cargado: {format(nrow(baseline_dt), big.mark=',')} registros")


  }, error = function(e) {
      log_error("Error cargando datos auxiliares: {e$message}")
      return(list(baseline = NULL)) # Devolver lista vacía o NULL
  }, finally = {
      if (!is.null(con_aux) && dbIsValid(con_aux)) dbDisconnect(con_aux)
  })

  return(list(baseline = baseline_dt))
}

# --- Feature Engineering  ---
crear_variables_derivadas_prediccion <- function(datos_combinados, datos_auxiliares) {
  log_info("Creando variables derivadas para predicción (estilo XGBoost)...")
  setDT(datos_combinados) 

  madrid_centro_utm_x <- 440000L; madrid_centro_utm_y <- 4474000L

  # 1. Mapear contaminante a id_magnitud
  datos_combinados[, id_magnitud := fcase(
      contaminante == "Dióxido de Nitrógeno", 8L,
      contaminante == "Partículas < 10 µm", 9L,
      contaminante == "Partículas < 2.5 µm", 10L,
      contaminante == "Ozono", 14L,
      contaminante == "Dióxido de Azufre", 1L,
      default = NA_integer_
  )]

  # 2. Variables espaciales UTM
  if ("lon" %in% names(datos_combinados) && "lat" %in% names(datos_combinados) && requireNamespace("sf", quietly = TRUE)) {
      log_info("Calculando coordenadas UTM desde lon/lat...")
      coords_sf <- sf::st_as_sf(datos_combinados[!is.na(lon) & !is.na(lat)], coords = c("lon", "lat"), crs = 4326)
      coords_utm <- sf::st_transform(coords_sf, 25830)
      utm_coords_mat <- sf::st_coordinates(coords_utm)
      # Asignar por referencia usando un join temporal o índices (más seguro)
      # Crear un índice temporal para el join
      datos_combinados[, temp_id := .I]
      coords_dt <- data.table(temp_id = datos_combinados[!is.na(lon) & !is.na(lat), temp_id], utm_x = utm_coords_mat[,1], utm_y = utm_coords_mat[,2])
      datos_combinados[coords_dt, on = "temp_id", `:=`(utm_x = i.utm_x, utm_y = i.utm_y)]
      datos_combinados[, temp_id := NULL] # Limpiar índice temporal
      # Rellenar NAs con el centro (si la conversión falló o lat/lon eran NA)
      datos_combinados[is.na(utm_x), utm_x := madrid_centro_utm_x]
      datos_combinados[is.na(utm_y), utm_y := madrid_centro_utm_y]
      rm(coords_sf, coords_utm, utm_coords_mat, coords_dt) # Limpiar memoria
  } else {
      log_warn("Columnas 'lon'/'lat' no encontradas o paquete 'sf' no disponible. Usando centro de Madrid como default UTM.")
      datos_combinados[, `:=`(utm_x = as.numeric(madrid_centro_utm_x), utm_y = as.numeric(madrid_centro_utm_y))]
  }

  # 3. Derivadas espaciales, temporales y meteorológicas (EXACTAMENTE como en training)
  # Bloque 1: Variables base (espaciales y temporales)
  datos_combinados[, `:=`(
    # Espaciales derivadas
    dist_centro_madrid = sqrt((utm_x - madrid_centro_utm_x)^2 + (utm_y - madrid_centro_utm_y)^2) / 1000,
    utm_x_norm = (utm_x - madrid_centro_utm_x) / 10000,
    utm_y_norm = (utm_y - madrid_centro_utm_y) / 10000,
    # Temporales base
    año = year(fecha_hora),
    mes = month(fecha_hora),
    dia = mday(fecha_hora),
    dia_año = yday(fecha_hora),
    dia_semana = wday(fecha_hora, week_start=1)-1, # Lunes=0 a Domingo=6
    hora = hour(fecha_hora),
    # Meteo derivadas - Temp
    temp_abs = abs(temp_media_c),
    temp_sign = sign(temp_media_c),
    temp_sq = temp_media_c^2,
    temp_sq_signed = temp_media_c * abs(temp_media_c),
    temp_cubic = temp_media_c^3,
    # Meteo derivadas - Hum/Temp
    temp_hum_ratio = (temp_media_c + 20) / (humedad_media_pct + 1),
    temp_hum_ratio_inv = humedad_media_pct / (abs(temp_media_c) + 1),
    # Meteo derivadas - VPD
    e_sat = 6.112 * exp((17.67 * temp_media_c) / (temp_media_c + 243.5))
  )]

  # Bloque 2: Variables derivadas de las anteriores
  datos_combinados[, `:=`(
    fin_semana = fifelse(dia_semana %in% c(5, 6), 1L, 0L) # Usa dia_semana ya creado
  )]
   # Cálculos dependientes
   datos_combinados[, `:=`(
       e_actual = e_sat * (humedad_media_pct / 100),
       vpd = e_sat - (e_sat * (humedad_media_pct / 100)) # Asegurar cálculo correcto
   )]

  # 4. Variables opcionales (Crear si no existen con defaults)
  # Crear columnas faltantes con valores por defecto
  if (!"temp_maxima_c" %in% names(datos_combinados)) {
    datos_combinados[, temp_maxima_c := temp_media_c + 6]
  }
  if (!"temp_minima_c" %in% names(datos_combinados)) {
    datos_combinados[, temp_minima_c := temp_media_c - 6]
  }
  if (!"presion_maxima_hpa" %in% names(datos_combinados)) {
    datos_combinados[, presion_maxima_hpa := fcoalesce(presion_maxima_hpa, 1016)]
  }
  if (!"presion_minima_hpa" %in% names(datos_combinados)) {
    datos_combinados[, presion_minima_hpa := 1010]
  }
  if (!"presion_media_hpa" %in% names(datos_combinados)) {
    datos_combinados[, presion_media_hpa := 1013]
  }
  if (!"humedad_maxima_pct" %in% names(datos_combinados)) {
    datos_combinados[, humedad_maxima_pct := humedad_media_pct * 1.15]
  }
  if (!"humedad_minima_pct" %in% names(datos_combinados)) {
    datos_combinados[, humedad_minima_pct := humedad_media_pct * 0.85]
  }

  # Ahora crear derivadas y componentes viento
  datos_combinados[, `:=`(
      dir_viento = fcoalesce(as.numeric(dir_viento_grados), 180),
      temp_range = temp_maxima_c - temp_minima_c,
      presion_diff = presion_maxima_hpa - presion_minima_hpa,
      humedad_diff = pmax(0, humedad_maxima_pct - humedad_minima_pct) # Evitar negativos
  )]

  # Componentes viento (usando dir_viento ya calculado)
  datos_combinados[, `:=`(
      viento_x = vel_viento_media_ms * cos(dir_viento * pi / 180),
      viento_y = vel_viento_media_ms * sin(dir_viento * pi / 180)
  )]


  # 5. Tipo de día
  datos_combinados[, tipo_dia_num := fcase(
      dia_semana %in% c(5, 6), 0L, # Asumiendo Sab=5, Dom=6
      dia_semana == 0, 1L,       # Lunes=0
      dia_semana == 4, 2L,       # Viernes=4
      default = 3L               # Mar-Jue
  )]

  # 6. Join con Baseline
  if (!is.null(datos_auxiliares$baseline)) {
    log_info("Uniendo con baseline estacional (promedio_5y)...")
    # Asegurar que las columnas clave existen y tienen el tipo correcto
    datos_combinados[, `:=`(mes = as.integer(mes), dia = as.integer(dia), hora = as.integer(hora))]
    datos_auxiliares$baseline[, `:=`(mes = as.integer(mes), dia_mes = as.integer(dia_mes), hora = as.integer(hora))]

    # Join usando claves (establecidas en cargar_datos_auxiliares)
    datos_combinados[datos_auxiliares$baseline, on = .(id_magnitud, mes, dia = dia_mes, hora),
                     promedio_5y := i.promedio_5y]

    n_con_baseline <- datos_combinados[!is.na(promedio_5y), .N]
    if (n_con_baseline < nrow(datos_combinados)) {
        log_warn("No se encontró baseline para {nrow(datos_combinados) - n_con_baseline} registros. Se imputarán con NA.")
        # Opcional: imputar NA de baseline con media global o similar si el modelo lo requiere
        # datos_combinados[is.na(promedio_5y), promedio_5y := mean(promedio_5y, na.rm = TRUE)]
    }
    log_info("✅ Baseline estacional agregado.")
  } else {
    log_warn("⚠️ Baseline no disponible, columna 'promedio_5y' será NA.")
    datos_combinados[, promedio_5y := NA_real_]
  }

  # Comprobar Inf/NaN (después de TODOS los cálculos)
  log_info("Comprobando Inf/NaN finales...")
  cols_numeric_final <- names(datos_combinados)[sapply(datos_combinados, is.numeric)]
  for (col in cols_numeric_final) {
    idx_nonfinite <- which(!is.finite(datos_combinados[[col]]))
    if (length(idx_nonfinite) > 0) {
      log_warn("  Detectados {length(idx_nonfinite)} Inf/NaN en '{col}' (final). Reemplazando con NA.")
      set(datos_combinados, i = idx_nonfinite, j = col, value = NA_real_)
    }
  }

  log_info("Variables derivadas creadas.")
  return(datos_combinados)
}


# --- Generación de Predicciones (Adaptado a xgboost nativo) ---
generar_predicciones_estaciones <- function(datos_meteo, datos_auxiliares, modelos_dir = MODELOS_DIR) {
  log_info("Generando predicciones XGBoost nativo para estaciones de Madrid...")
  memoria_inicial_mb <- sum(gc()[, 2])
  log_info("💾 Memoria inicial: {round(memoria_inicial_mb, 1)} MB")

  # Cargar métricas de CV si existen (opcional, para reportar)
  metricas_file <- file.path(modelos_dir, "xgboost_nativo_ica_metricas.rds")
  metricas_cv <- NULL
  if(file.exists(metricas_file)) {
      metricas_cv <- readRDS(metricas_file)
      setDT(metricas_cv)
      log_info("📊 Métricas de CV cargadas.")
  }

  # Lista de modelos nativos .model
  archivos_modelos <- list.files(modelos_dir, pattern = "^xgboost_nativo_ica_.*\\.model$", full.names = TRUE)
  if (length(archivos_modelos) == 0) {
    log_error("No se encontraron archivos .model en '{modelos_dir}'")
    return(NULL)
  }
  log_info("📦 Cargando modelos xgboost nativos (.model) uno por uno: {length(archivos_modelos)} encontrados.")

  # Coordenadas de estaciones (data.table)
  estaciones_dt <- data.table(
    id_estacion = c(4, 8, 11, 16, 17, 18, 27, 35, 36, 38, 39, 40, 47, 48, 49, 50),
    nombre_estacion = c("Pza. de España", "Escuelas Aguirre", "Av. Ramón y Cajal", "Arturo Soria",
                      "Villaverde Alto", "Farolillo", "Barajas Pueblo", "Pza. del Carmen", "Moratalaz",
                      "Cuatro Caminos", "Barrio del Pilar", "Vallecas", "Mendez Alvaro", "Castellana",
                      "Retiro", "Pza. Castilla"),
    lat = c(40.4238, 40.4213, 40.4514, 40.4405, 40.3479, 40.3748, 40.4756, 40.4192, 40.4077,
            40.4459, 40.4773, 40.3943, 40.3980, 40.4407, 40.4152, 40.4656),
    lon = c(-3.7122, -3.6958, -3.6774, -3.6394, -3.7215, -3.7336, -3.5935, -3.7026, -3.6453,
            -3.7097, -3.7137, -3.6458, -3.6862, -3.6889, -3.6823, -3.6951)
  )

  predicciones_finales_list <- list() # Usar lista para rbindlist al final

  # Bucle por modelo
  for (i in seq_along(archivos_modelos)) {
    archivo_model <- archivos_modelos[i]
    nombre_contaminante <- gsub("^xgboost_nativo_ica_|_", " ", basename(archivo_model))
    nombre_contaminante <- gsub("\\.model$", "", nombre_contaminante)
    # Mapeo robusto a nombres oficiales
    nombre_contaminante <- fcase(
        grepl("Di.*xido.*Nitr", nombre_contaminante, ignore.case=T), "Dióxido de Nitrógeno",
        grepl("Part.*culas.*10", nombre_contaminante, ignore.case=T), "Partículas < 10 µm",
        grepl("Part.*culas.*2.*5", nombre_contaminante, ignore.case=T), "Partículas < 2.5 µm",
        grepl("Ozono", nombre_contaminante, ignore.case=T), "Ozono",
        grepl("Di.*xido.*Azufre", nombre_contaminante, ignore.case=T), "Dióxido de Azufre",
        default = nombre_contaminante # Fallback
    )

    log_info("📦 [{i}/{length(archivos_modelos)}] Procesando: {nombre_contaminante}...")

    tryCatch({
      # Cargar modelo xgboost nativo
      modelo_xgb <- xgb.load(archivo_model)
      memoria_modelo_mb <- sum(gc()[, 2])
      log_info("  💾 Memoria tras cargar modelo: {round(memoria_modelo_mb, 1)} MB")

      # Crear combinaciones (data.table)
      # Usar CJ (cross join) de data.table para eficiencia
      combinaciones_dt <- CJ(
          fecha_hora = datos_meteo$fecha_hora,
          id_estacion = estaciones_dt$id_estacion,
          sorted = FALSE # Más rápido si el orden no importa aquí
      )
      combinaciones_dt[, contaminante := nombre_contaminante]
      # Joins eficientes con data.table
      combinaciones_dt[datos_meteo, on = "fecha_hora", names(datos_meteo)[!names(datos_meteo) %in% "fecha_hora"] := mget(paste0("i.", names(datos_meteo)[!names(datos_meteo) %in% "fecha_hora"]))]
      combinaciones_dt[estaciones_dt, on = "id_estacion", `:=`(nombre_estacion = i.nombre_estacion, lon = i.lon, lat = i.lat)]

      # Crear variables derivadas (EXACTAMENTE como en training)
      datos_prediccion <- crear_variables_derivadas_prediccion(combinaciones_dt, datos_auxiliares)

      # Obtener nombres de predictores del modelo cargado (robustez)
      feature_names_model <- modelo_xgb$feature_names
      if (is.null(feature_names_model)) {
          log_warn("No se pudieron obtener feature_names del modelo, usando nombres del script de training.")
          # Definir aquí los nombres EXACTOS y en el ORDEN del training
           feature_names_model <- c( "hora", "dia_semana", "mes", "dia_año", "fin_semana", "tipo_dia_num",
                                     "temp_media_c", "precipitacion_mm", "vel_viento_media_ms", "viento_x", "viento_y",
                                     "temp_abs", "temp_sign", "temp_sq", "temp_sq_signed", "temp_cubic",
                                     "temp_hum_ratio", "temp_hum_ratio_inv", "e_sat", "e_actual", "vpd",
                                     "utm_x", "utm_y", "utm_x_norm", "utm_y_norm", "dist_centro_madrid",
                                     "promedio_5y",
                                     # Añadir opcionales SIEMPRE en el mismo orden que training
                                     "temp_range", "presion_diff", "humedad_diff", "dir_viento",
                                     "presion_maxima_hpa", "presion_minima_hpa", "presion_media_hpa"
                                    )
           # Filtrar por los que realmente existen en datos_prediccion
           feature_names_model <- intersect(feature_names_model, names(datos_prediccion))

      }
      log_info("  Usando {length(feature_names_model)} predictores del modelo.")

      # Asegurar que todas las columnas existen y están en el orden correcto
      missing_cols <- setdiff(feature_names_model, names(datos_prediccion))
      if(length(missing_cols) > 0) {
          log_error("Faltan columnas requeridas por el modelo: {paste(missing_cols, collapse=', ')}")
          stop("Columnas faltantes.")
      }
      # Crear matriz numérica en el orden correcto
      prediccion_matrix <- as.matrix(datos_prediccion[, ..feature_names_model])

      # Comprobar NAs finales ANTES de predecir
      if(anyNA(prediccion_matrix)){
          log_error("Se encontraron NAs en la matriz final antes de predecir. Revisar imputación.")
           # Opcional: imputar aquí como último recurso, pero es mejor arreglarlo antes
           # prediccion_matrix[is.na(prediccion_matrix)] <- 0 # O imputar con mediana/media
          stop("NAs encontrados en matriz de predicción.")
      }


      # PREDICCIONES (xgboost nativo)
      log_info("  Realizando predicciones...")
      predicciones_xgb <- predict(modelo_xgb, newdata = prediccion_matrix)

      # Recuperar métricas de CV si están disponibles
      rmse_cv_modelo <- NA_real_; r2_cv_modelo <- NA_real_ # Defaults
      if (!is.null(metricas_cv)) {
          metricas_contam <- metricas_cv[contaminante == nombre_contaminante]
          if(nrow(metricas_contam) > 0) {
              rmse_cv_modelo <- metricas_contam$RMSE_cv[1]
              r2_cv_modelo <- metricas_contam$Rsquared_cv[1] # Será NA si no se calculó
          }
      }

      # Ensamblar resultados (data.table)
      resultados_contam <- datos_prediccion[, .(
          id_estacion, nombre_estacion, lat, lon, fecha_hora, contaminante,
          prediccion = predicciones_xgb,
          rmse_modelo_cv = rmse_cv_modelo # RMSE de CV
          # r2_modelo_cv = r2_cv_modelo # R2 (puede ser NA)
      )]

      predicciones_finales_list[[nombre_contaminante]] <- resultados_contam

      log_success("✓ {nombre_contaminante}: {nrow(resultados_contam)} predicciones generadas.")
      log_info("  Rango: {round(min(predicciones_xgb), 1)} - {round(max(predicciones_xgb), 1)} µg/m³")
      log_info("  RMSE (CV): {round(rmse_cv_modelo, 2)}")

      # Liberar memoria (modelo y datos temporales)
      rm(modelo_xgb, combinaciones_dt, datos_prediccion, prediccion_matrix, predicciones_xgb, resultados_contam)
      gc_info <- gc(verbose=FALSE)
      memoria_despues_mb <- sum(gc_info[, 2])
      log_info("  💾 Memoria tras liberar: {round(memoria_despues_mb, 1)} MB")

    }, error = function(e) {
      log_error("❌ Error con {nombre_contaminante}: {e$message}")
      # Opcional: log stack trace
      # log_error("Stack trace: {paste(deparse(sys.calls()), collapse = '\n')}")
    })
  } # --- Fin bucle modelos ---

  # Combinar todas las predicciones
  if (length(predicciones_finales_list) == 0) {
    log_error("No se generaron predicciones exitosamente para ningún contaminante.")
    return(NULL)
  }
  resultado_dt <- rbindlist(predicciones_finales_list)

  # Convertir a objeto espacial sf (si es necesario y sf está disponible)
  resultado_final <- resultado_dt
  if (requireNamespace("sf", quietly = TRUE)) {
      log_info("Convirtiendo resultados a objeto espacial 'sf'...")
      resultado_final <- sf::st_as_sf(resultado_dt[!is.na(lon) & !is.na(lat)],
                                     coords = c("lon", "lat"), crs = 4326)
      resultado_final$timestamp_prediccion <- Sys.time() # Añadir timestamp
  } else {
      log_warn("Paquete 'sf' no disponible. Devolviendo data.table.")
      resultado_final[, timestamp_prediccion := Sys.time()]
  }


  log_success("✓ Predicciones horarias completadas: {nrow(resultado_final)} registros")
  return(resultado_final)
}

# --- FUNCIÓN PRINCIPAL (Wrapper) ---
generar_predicciones_40h <- function(horas = N_HORAS_PREDICCION) {
  log_info("=== GENERANDO PREDICCIONES PRÓXIMAS {horas} HORAS (XGBoost Nativo) ===")
  datos_meteo <- obtener_meteo_aemet(horas)
  if (is.null(datos_meteo) || nrow(datos_meteo) == 0) {
    log_error("No se pudieron obtener/generar datos meteorológicos.")
    return(NULL)
  }

  datos_auxiliares <- cargar_datos_auxiliares()
  if (is.null(datos_auxiliares$baseline)) {
      log_warn("Continuando sin datos de baseline...")
      # Asegurarse de que sea una lista válida para la función de predicción
      datos_auxiliares <- list(baseline = data.table()) # Pasar DT vacío
  }


  predicciones <- generar_predicciones_estaciones(datos_meteo, datos_auxiliares, modelos_dir = MODELOS_DIR)

  if (is.null(predicciones) || nrow(predicciones) == 0) {
    log_error("Fallo al generar predicciones.")
    return(NULL)
  }

  # Guardar resultados
  dir.create("output", showWarnings = FALSE) # Crear directorio si no existe
  archivo_salida <- "output/predicciones_xgb_nativo_40h_latest.rds"
  saveRDS(predicciones, archivo_salida)

  archivo_meteo <- "output/meteo_40h_latest.rds"
  saveRDS(datos_meteo, archivo_meteo)

  log_success("✓ PREDICCIONES XGB NATIVO 40H COMPLETADAS")
  log_info("  Total registros: {nrow(predicciones)}")
  log_info("  Archivo predicciones: {archivo_salida}")
  log_info("  Archivo meteorología: {archivo_meteo}")

  return(predicciones)
}

# --- FUNCIÓN DE TEST (Adaptada) ---
test_predicciones_40h_xgb_nativo <- function() {
  log_info("=== TEST PREDICCIONES 40H (XGBoost Nativo) ===")
  resultado <- generar_predicciones_40h(N_HORAS_PREDICCION)

  if (is.null(resultado) || nrow(resultado) == 0) {
    log_error("❌ Test falló: No se generaron resultados.")
    return(FALSE)
  }

  # Validaciones básicas
  n_registros <- nrow(resultado)
  horas_unicas <- uniqueN(resultado$fecha_hora)
  estaciones_unicas <- uniqueN(resultado$id_estacion)
  contaminantes_unicos <- uniqueN(resultado$contaminante)
  pred_range <- range(resultado$prediccion, na.rm = TRUE)
  n_negativos <- sum(resultado$prediccion < 0, na.rm = TRUE)
  n_na <- sum(is.na(resultado$prediccion))

  log_success("✓ Test predicciones 40h (XGB Nativo) parece exitoso.")
  log_info("  Registros totales: {n_registros}")
  log_info("  Horas únicas: {horas_unicas}")
  log_info("  Estaciones únicas: {estaciones_unicas}")
  log_info("  Contaminantes únicos: {contaminantes_unicos}")
  log_info("  Rango predicciones: {round(pred_range[1], 1)} - {round(pred_range[2], 1)}")
  log_info("  Valores negativos: {n_negativos}")
  log_info("  Valores NA: {n_na}")

  if(n_negativos > 0) log_warn("Se encontraron predicciones negativas.")
  if(n_na > 0) log_error("Se encontraron NAs en las predicciones finales.")

  return(n_na == 0) # Test pasa si no hay NAs finales
}

# --- Ejecución ---
tryCatch({
  if (!interactive()) {
    test_result <- test_predicciones_40h_xgb_nativo()
    # Opcional: Salir con código de error si el test falla en CI/CD
    # if(!test_result) q(status = 1)
  }

}, error = function(e) { # tryCatch global
    log_error("Error fatal en el script principal de predicciones: {e$message}")
    log_error("Stack trace: {paste(deparse(sys.calls()), collapse = '\n')}")
}, finally = {
    log_info("--- FIN PREDICCIONES HORARIAS (XGBOOST NATIVO) ---")
})