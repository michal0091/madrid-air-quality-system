#!/usr/bin/env Rscript
# SCRIPT 06: MODELO XGBOOST OPTIMIZADO PARA ICA (5 CONTAMINANTES)
# --------------------------------------------------------------------
# Objetivo: Entrenar modelos con xgboost + GPU + data.table puro
# Ventajas: 10-20x más pequeño, 10-50x más rápido, usa GPU RTX 4070Ti
# Hardware: 16 cores CPU, 61GB RAM, RTX 4070Ti
# --------------------------------------------------------------------


# ==================== LIBRERÍAS ====================
library(data.table)    # Manipulación de datos eficiente
library(xgboost)       # Gradient Boosting con GPU
library(caret)         # ML framework (wrapper para xgboost)
library(logger)        # Logging estructurado
library(DBI)           # Database interface
library(RPostgres)     # PostgreSQL connector
library(glue)          # String interpolation

# ==================== CONFIGURACIÓN ====================
log_threshold(INFO)
log_appender(appender_tee("logs/modelo_xgboost_ica.log"))
log_info("=== MODELO XGBOOST ICA - 5 CONTAMINANTES ===")

# Verificar GPU disponible
gpu_disponible <- system("nvidia-smi",
                         ignore.stdout = TRUE,
                         ignore.stderr = TRUE) == 0
if (gpu_disponible) {
  log_success("✅ GPU detectada - xgboost usará tree_method='gpu_hist'")
} else {
  log_warn("⚠️ GPU no detectada - xgboost usará CPU")
}

# Parámetros optimizados
PORCENTAJE_MUESTRA <- 1.0  # 100% del dataset
SEED <- 42
set.seed(SEED)

# Cargar utilidades de expansión meteorológica
if (file.exists("R/utils_meteo_horario.R")) {
  source("R/utils_meteo_horario.R")
  log_info("✅ utils_meteo_horario.R cargado")
} else {
  log_warn("⚠️ utils_meteo_horario.R no encontrado")
}

# ==================== INICIO DEL BLOQUE TRYCATCH ====================
tryCatch({
  # ==================== CONEXIÓN BD ====================
  readRenviron(".Renviron")
  
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    port = Sys.getenv("DB_PORT"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
  log_success("✅ Conectado a PostgreSQL en {Sys.getenv('DB_HOST')}")
  
  # ==================== QUERY DATOS REALES ====================
  query_ica <- glue(
    "
  SELECT
    fm.fecha_hora,
    fm.valor_medido as valor_medio,
    fm.id_magnitud,
    fm.id_estacion,
    de.\"LONGITUD\"::FLOAT as longitud,
    de.\"LATITUD\"::FLOAT as latitud,
    ST_X(ST_Transform(ST_SetSRID(ST_MakePoint(de.\"LONGITUD\"::FLOAT, de.\"LATITUD\"::FLOAT), 4326), 25830)) as utm_x,
    ST_Y(ST_Transform(ST_SetSRID(ST_MakePoint(de.\"LONGITUD\"::FLOAT, de.\"LATITUD\"::FLOAT), 4326), 25830)) as utm_y,
    dm.descripcion as contaminante,
    dm.unidad_medida as unidad,
    de.nombre_estacion,
    de.tipo_estacion,
    EXTRACT(YEAR FROM fm.fecha_hora) as año,
    EXTRACT(MONTH FROM fm.fecha_hora) as mes,
    EXTRACT(DAY FROM fm.fecha_hora) as dia,
    EXTRACT(HOUR FROM fm.fecha_hora) as hora,
    EXTRACT(DOY FROM fm.fecha_hora) as dia_año,
    EXTRACT(DOW FROM fm.fecha_hora) as dia_semana,
    DATE(fm.fecha_hora) as fecha,
    fmd.temp_media_c,
    fmd.temp_maxima_c,
    fmd.temp_minima_c,
    fmd.precipitacion_mm,
    fmd.vel_viento_media_ms,
    fmd.dir_viento_grados,
    fmd.presion_maxima_hpa,
    fmd.presion_minima_hpa,
    fmd.humedad_media_pct,
    fmd.humedad_maxima_pct,
    fmd.humedad_minima_pct
  FROM fact_mediciones fm
  JOIN dim_estaciones de ON fm.id_estacion = de.id_estacion
  JOIN dim_magnitudes dm ON fm.id_magnitud = dm.id_magnitud
  LEFT JOIN fact_meteo_diaria fmd ON DATE(fm.fecha_hora) = fmd.fecha
  WHERE fm.fecha_hora >= '2015-01-01'
    AND fm.fecha_hora < '2025-10-01'
    AND fm.valor_medido IS NOT NULL
    AND fm.valor_medido >= 0
    AND fmd.temp_media_c IS NOT NULL
    AND dm.descripcion IN (
      'Dióxido de Nitrógeno',
      'Partículas < 10 µm',
      'Partículas < 2.5 µm',
      'Ozono',
      'Dióxido de Azufre'
    )
  ORDER BY fm.fecha_hora
"
  )
  
  log_info("📊 Cargando datos 2015-2025 (10 años, 5 contaminantes ICA)...")
  inicio_carga <- Sys.time()
  
  datos_dt <- setDT(dbGetQuery(con, query_ica))
  
  # ==================== CARGAR BASELINE ESTACIONAL ====================
  log_info("\nCargando baseline estacional desde BD...")
  
  baseline_estacional <- setDT(
    dbGetQuery(
      con,
      "
  SELECT
    id_magnitud,
    mes,
    dia_mes,
    hora,
    promedio_5y,
    p10,
    p90
  FROM dim_baseline_estacional
"
    )
  )
  
  log_success("✅ Baseline cargado: {format(nrow(baseline_estacional), big.mark=',')} registros")
  dbDisconnect(con)
  
  tiempo_carga <- difftime(Sys.time(), inicio_carga, units = "secs")
  log_success(
    "✅ Datos cargados: {format(nrow(datos_dt), big.mark=',')} obs en {round(tiempo_carga, 1)}s"
  )
  log_info("Período: {min(datos_dt$fecha)} a {max(datos_dt$fecha)}")
  
  # Distribución por contaminante
  dist_contaminantes <- datos_dt[, .(n = .N), by = contaminante][order(-n)]
  log_info("\nDistribución de observaciones por contaminante:")
  log_info(
    "  {dist_contaminantes$contaminante}: {format(dist_contaminantes$n, big.mark=',')}"
  )
  
  
  # ==================== EXPANSIÓN METEOROLÓGICA ====================
  log_info("\n=== EXPANDIENDO DATOS METEOROLÓGICOS (data.table) ===")
  
  cols_meteo <- c(
    "temp_media_c",
    "temp_maxima_c",
    "temp_minima_c",
    "precipitacion_mm",
    "vel_viento_media_ms",
    "dir_viento_grados",
    "presion_maxima_hpa",
    "presion_minima_hpa",
    "humedad_media_pct",
    "humedad_maxima_pct",
    "humedad_minima_pct"
  )
  
  datos_meteo_diarios <- unique(datos_dt[, .SD, .SDcols = c("fecha", cols_meteo)])
  datos_meteo_diarios <- datos_meteo_diarios[, .SD[1], fecha]
  
  log_info("Fechas únicas con datos meteorológicos: {nrow(datos_meteo_diarios)}")
  
  if (exists("expandir_meteo_sinusoidal")) {
    datos_meteo_horarios <- expandir_meteo_sinusoidal(datos_meteo_diarios)
    log_success(
      "✅ Expansión SINUSOIDAL: {format(nrow(datos_meteo_horarios), big.mark=',')} registros"
    )
  } else if (exists("expandir_meteo_lineal")) {
    log_warn("⚠️ Usando expansión lineal (sinusoidal no disponible)")
    datos_meteo_horarios <- expandir_meteo_lineal(datos_meteo_diarios)
    log_success(
      "✅ Expansión lineal: {format(nrow(datos_meteo_horarios), big.mark=',')} registros"
    )
  } else {
    log_error("❌ Funciones de expansión meteorológica no encontradas")
    stop("Expansión meteorológica fallida")
  }
  
  setDT(datos_meteo_horarios)
  
  setnames(
    datos_meteo_horarios,
    old = c(
      "temp_c",
      "humedad_pct",
      "presion_hpa",
      "vel_viento_ms",
      "precipitacion_mm"
    ),
    new = c(
      "temp_media_c_horario",
      "humedad_media_pct_horario",
      "presion_media_hpa",
      "vel_viento_media_ms_horario",
      "precipitacion_mm_horario"
    ),
    skip_absent = TRUE
  )
  
  # ==================== JOIN METEOROLOGÍA ====================
  log_info("\n=== JOIN METEOROLOGÍA HORARIA ===")
  
  setkeyv(datos_dt, c("fecha", "hora"))
  setkeyv(datos_meteo_horarios, c("fecha", "hora"))
  
  datos_completos <- datos_meteo_horarios[datos_dt, on = .(fecha, hora)]
  
  datos_completos[, `:=`(
    temp_media_c = fcoalesce(temp_media_c_horario, temp_media_c),
    humedad_media_pct = fcoalesce(humedad_media_pct_horario, humedad_media_pct),
    vel_viento_media_ms = fcoalesce(vel_viento_media_ms_horario, vel_viento_media_ms),
    precipitacion_mm = fcoalesce(precipitacion_mm_horario, precipitacion_mm)
  )]
  
  cols_temp <- c(
    "temp_media_c_horario",
    "humedad_media_pct_horario",
    "vel_viento_media_ms_horario",
    "precipitacion_mm_horario"
  )
  datos_completos[, (cols_temp) := NULL]
  
  datos_completos <- datos_completos[!is.na(temp_media_c)]
  log_info("Datos tras JOIN: {format(nrow(datos_completos), big.mark=',')} observaciones")
  
  # ==================== MUESTREO ESTRATIFICADO ====================
  if (PORCENTAJE_MUESTRA < 1.0) {
    log_info("\n=== MUESTREO ESTRATIFICADO (data.table) ===")
    log_info("Estratificación por: contaminante + año + mes")
    
    indices_muestra <- datos_completos[, .I[sample(.N, .N * PORCENTAJE_MUESTRA)], by = .(contaminante, año, mes)]$V1
    
    datos_muestra <- datos_completos[indices_muestra]
    log_success("✅ Muestra generada: {format(nrow(datos_muestra), big.mark=',')} observaciones")
  } else {
    log_info("\n📊 USANDO DATASET COMPLETO (100%)")
    datos_muestra <- datos_completos
  }
  
  dist_muestra <- datos_muestra[, .(n = .N,
                                    porcentaje = .N / nrow(datos_muestra) * 100), by = contaminante][order(-n)]
  log_info("\nDistribución muestra por contaminante:")
  log_info(
    "  {dist_muestra$contaminante}: {format(dist_muestra$n, big.mark=',')} ({round(dist_muestra$porcentaje, 1)}%)"
  )
  
  # ==================== FEATURE ENGINEERING  ====================
  log_info("\n=== PREPARANDO DATOS ML ===")
  
  madrid_centro_utm_x <- 440000L
  madrid_centro_utm_y <- 4474000L
  
  datos_muestra[, `:=`(
    # Conversiones primero
    utm_x = as.numeric(utm_x),
    utm_y = as.numeric(utm_y),
    
    # Derivadas (asegurando que utm_x/y se usen como numéricos)
    dist_centro_madrid = sqrt((as.numeric(utm_x) - madrid_centro_utm_x)^2 +
                                (as.numeric(utm_y) - madrid_centro_utm_y)^2
    ) / 1000,
    utm_x_norm = (as.numeric(utm_x) - madrid_centro_utm_x) / 10000,
    utm_y_norm = (as.numeric(utm_y) - madrid_centro_utm_y) / 10000,
    
    fin_semana = fifelse(dia_semana %in% c(0, 6), 1L, 0L),
    
    temp_abs = abs(temp_media_c),
    temp_sign = sign(temp_media_c),
    temp_sq = temp_media_c^2,
    temp_sq_signed = temp_media_c * abs(temp_media_c),
    temp_cubic = temp_media_c^3,
    
    temp_hum_ratio = (temp_media_c + 20) / (humedad_media_pct + 1),
    temp_hum_ratio_inv = humedad_media_pct / (abs(temp_media_c) + 1),
    
    e_sat = 6.112 * exp((17.67 * temp_media_c) / (temp_media_c + 243.5)),
    
    
    presion_diff = fifelse(
      !is.na(presion_maxima_hpa) & !is.na(presion_minima_hpa),
      presion_maxima_hpa - presion_minima_hpa,
      NA_real_
    ),
    humedad_diff = fifelse(
      !is.na(humedad_maxima_pct) & !is.na(humedad_minima_pct),
      humedad_maxima_pct - humedad_minima_pct,
      NA_real_
    ),
    temp_range = fifelse(
      !is.na(temp_maxima_c) & !is.na(temp_minima_c),
      temp_maxima_c - temp_minima_c,
      NA_real_
    ),
    
    dir_viento = fcoalesce(dir_viento_grados, 180)
  )]
  
  # Variables derivadas de las anteriores
  datos_muestra[, `:=`(
    e_actual = e_sat * (humedad_media_pct / 100),
    vpd = e_sat - e_sat * (humedad_media_pct / 100),
    viento_x = vel_viento_media_ms * cos(dir_viento * pi / 180),
    viento_y = vel_viento_media_ms * sin(dir_viento * pi / 180)
  )]
  
  
  log_info("Checking for non-finite values (Inf, NaN) after feature engineering...")
  cols_numeric <- names(datos_muestra)[sapply(datos_muestra, is.numeric)]
  
  # Loop through numeric columns and replace Inf with NA, NaN with NA
  # We do this BEFORE the baseline join and final filtering/imputation
  n_inf_nan_replaced <- 0
  for (col in cols_numeric) {
    # Check specifically for Inf or NaN
    idx_nonfinite <- which(!is.finite(datos_muestra[[col]]))
    if (length(idx_nonfinite) > 0) {
      log_warn(
        "Found {length(idx_nonfinite)} Inf/NaN values in column '{col}'. Replacing with NA."
      )
      # Replace by reference using set() for efficiency
      set(datos_muestra,
          i = idx_nonfinite,
          j = col,
          value = NA)
      n_inf_nan_replaced <- n_inf_nan_replaced + length(idx_nonfinite)
    }
  }
  
  if (n_inf_nan_replaced > 0) {
    log_success("Replaced a total of {n_inf_nan_replaced} Inf/NaN values with NA.")
  } else {
    log_info("No Inf or NaN values found.")
  }
  
  
  # ==================== BASELINE ESTACIONAL ====================
  log_info("\n=== AGREGANDO BASELINE ESTACIONAL (data.table join) ===")
  
  setkeyv(baseline_estacional,
          c("id_magnitud", "mes", "dia_mes", "hora"))
  setkeyv(datos_muestra, c("id_magnitud", "mes", "dia", "hora"))
  
  datos_ml <- baseline_estacional[datos_muestra, on = .(id_magnitud, mes, dia_mes = dia, hora)]
  
  datos_ml[, tipo_dia_num := fcase(dia_semana %in% c(0, 6),
                                   0L,
                                   dia_semana == 1,
                                   1L,
                                   dia_semana == 5,
                                   2L,
                                   default = 3L)]
  
  datos_ml <- datos_ml[!is.na(promedio_5y)]
  log_success("✅ Baseline agregado: {format(nrow(datos_ml), big.mark=',')} observaciones")
  
  # ==================== SELECCIÓN FINAL DE COLUMNAS ====================
  cols_predictores <- c(
    "valor_medio",
    "contaminante",
    "hora",
    "dia_semana",
    "mes",
    "dia_año",
    "fin_semana",
    "tipo_dia_num",
    "temp_media_c",
    "precipitacion_mm",
    "vel_viento_media_ms",
    "viento_x",
    "viento_y",
    "temp_abs",
    "temp_sign",
    "temp_sq",
    "temp_sq_signed",
    "temp_cubic",
    "temp_hum_ratio",
    "temp_hum_ratio_inv",
    "e_sat",
    "e_actual",
    "vpd",
    "utm_x",
    "utm_y",
    "utm_x_norm",
    "utm_y_norm",
    "dist_centro_madrid",
    "promedio_5y"
  )
  
  cols_opcionales <- c(
    "temp_range",
    "presion_diff",
    "humedad_diff",
    "dir_viento",
    "presion_maxima_hpa",
    "presion_minima_hpa",
    "presion_media_hpa"
  )
  cols_disponibles <- intersect(cols_opcionales, names(datos_ml))
  cols_finales <- c(cols_predictores, cols_disponibles)
  
  datos_ml <- datos_ml[, .SD, .SDcols = cols_finales]
  
  datos_ml <- datos_ml[!is.na(valor_medio) & !is.na(temp_media_c) &
                         !is.na(utm_x) &
                         !is.na(utm_y) & !is.na(promedio_5y)]
  
  # Imputar NAs
  for (col in cols_disponibles) {
    if (anyNA(datos_ml[[col]])) {
      if (col == "temp_range")
        datos_ml[is.na(get(col)), (col) := 10]
      else if (col == "presion_diff")
        datos_ml[is.na(get(col)), (col) := 3]
      else if (col == "dir_viento")
        datos_ml[is.na(get(col)), (col) := 180]
      else if (col == "presion_maxima_hpa")
        datos_ml[is.na(get(col)), (col) := 1013]
      else if (col == "presion_minima_hpa")
        datos_ml[is.na(get(col)), (col) := 1010]
      else if (col == "presion_media_hpa")
        datos_ml[is.na(get(col)), (col) := 1011.5]
    }
  }
  
  cols_varianza <- names(datos_ml)[sapply(datos_ml, uniqueN) > 1]
  datos_ml <- datos_ml[, .SD, .SDcols = cols_varianza]
  
  datos_ml <- na.omit(datos_ml)
  
  n_predictores <- ncol(datos_ml) - 2
  log_success(
    "✅ Datos ML preparados: {format(nrow(datos_ml), big.mark=',')} obs, {n_predictores} predictores"
  )
  log_info("Estadísticas generales:")
  log_info("  Media concentración: {round(datos_ml[, mean(valor_medio)], 2)} µg/m³")
  log_info(
    "  Rango: {round(datos_ml[, min(valor_medio)], 2)} - {round(datos_ml[, max(valor_medio)], 2)} µg/m³"
  )
  
  # ==================== ENTRENAMIENTO XGBOOST NATIVO POR CONTAMINANTE ====================
  log_info("\n============================================================")
  log_info("INICIANDO ENTRENAMIENTO XGBOOST NATIVO - 5 CONTAMINANTES ICA")
  log_info("============================================================")
  
  # Grid adaptativo: más iteraciones para eta bajo, menos depth
  tune_grid <- rbind(
    # eta bajo = más iteraciones, menos profundidad
    expand.grid(nrounds = 300, max_depth = 6, eta = 0.01, 
                gamma = 0, colsample_bytree = 0.8, 
                min_child_weight = 1, subsample = 0.8),
    
    # eta medio = iteraciones medias
    expand.grid(nrounds = 200, max_depth = c(6, 8), eta = 0.05,
                gamma = 0, colsample_bytree = 0.8,
                min_child_weight = 1, subsample = 0.8),
    
    # eta alto = menos iteraciones, puede probar más profundidad
    expand.grid(nrounds = 100, max_depth = c(6, 8), eta = 0.1,
                gamma = 0, colsample_bytree = 0.8,
                min_child_weight = 1, subsample = 0.8)
  )
  
  log_info("Grid de hiperparámetros: {nrow(tune_grid)} combinaciones")
  
  modelos_xgb_nativo <- list() # Lista para guardar modelos xgb.Booster
  metricas_xgb <- list()      # Lista para guardar métricas
  
  contaminantes_ica <- c(
    "Dióxido de Nitrógeno",
    "Partículas < 10 µm",
    "Partículas < 2.5 µm",
    "Ozono",
    "Dióxido de Azufre"
  )
  
  # --- Bucle principal por contaminante ---
  for (contam in contaminantes_ica) {
    log_info("\n============================================================")
    log_info("CONTAMINANTE: {contam}")
    log_info("============================================================")
    
    # Filtrar datos del contaminante
    datos_contam <- datos_ml[contaminante == contam]
    
    log_info("Observaciones: {format(nrow(datos_contam), big.mark=',')}")
    
    if (nrow(datos_contam) == 0) {
      log_warn("⚠️ Sin observaciones para {contam} tras filtros. SALTANDO.")
      next
    }
    if (nrow(datos_contam) < 1000) {
      log_warn("⚠️ Menos de 1000 observaciones ({nrow(datos_contam)}), SALTANDO contaminante")
      next
    }
    
    log_info("Media: {round(datos_contam[, mean(valor_medio)], 2)} µg/m³")
    log_info("Desviación estándar: {round(datos_contam[, sd(valor_medio)], 2)} µg/m³")
    
    # --- Preparar datos para xgboost nativo ---
    log_info("Preparando datos para xgboost nativo (xgb.DMatrix)...")
    predictor_names <- setdiff(names(datos_contam), c("valor_medio", "contaminante"))
    x_matrix <- as.matrix(datos_contam[, ..predictor_names])
    y_vector <- datos_contam$valor_medio
    
    # Crear xgb.DMatrix (formato nativo de xgboost)
    dtrain <- xgb.DMatrix(data = x_matrix,
                          label = y_vector,
                          missing = NA)
    
    log_info("Iniciando búsqueda de hiperparámetros con xgb.cv...")
    cv_start_global <- Sys.time()
    
    best_cv_rmse <- Inf
    best_cv_params <- list()
    best_cv_nrounds <- NA_integer_
    all_cv_results <- list()
    
    # --- Bucle de Cross-Validation Manual ---
    for (i in 1:nrow(tune_grid)) {
      params <- list(
        objective = "reg:squarederror",
        eval_metric = "rmse",
        eta = tune_grid$eta[i],
        max_depth = tune_grid$max_depth[i],
        gamma = tune_grid$gamma[i],
        colsample_bytree = tune_grid$colsample_bytree[i],
        min_child_weight = tune_grid$min_child_weight[i],
        subsample = tune_grid$subsample[i],
        
        # GPU Configuration - RTX 4070 Ti
        tree_method = "hist",
        device = "cuda",
        
        # Optimización para GPU (RTX 4070 Ti tiene 12GB VRAM)
        max_bin = 256,
        # Más bins = mejor precisión (ajusta según memoria)
        grow_policy = "depthwise",
        # Mejor para GPU
        
        # Optimización para CPU (16 cores)
        nthread = 15,
        # Deja 1 core libre para el sistema
        
        # Opciones adicionales
        predictor = "gpu_predictor"  # Acelera las predicciones también
      )
      nrounds_candidate <- tune_grid$nrounds[i]
      
      log_info(
        "  CV Ronda {i}/{nrow(tune_grid)}: nrounds={nrounds_candidate}, max_depth={params$max_depth}, eta={params$eta}..."
      )
      cv_iter_start <- Sys.time()
      
      cv_model <- tryCatch({
        xgb.cv(
          params = params,
          data = dtrain,
          nrounds = nrounds_candidate,
          nfold = 5,
          showsd = TRUE,
          stratified = FALSE,
          verbose = 1,
          # Silenciar output interno de xgb.cv
          early_stopping_rounds = 10
        )
      }, error = function(e) {
        log_error("   !! Error en xgb.cv: {e$message}")
        NULL # Devolver NULL en caso de error
      })
      
      cv_iter_end <- Sys.time()
      
      if (!is.null(cv_model) && nrow(cv_model$evaluation_log) > 0) {
        current_best_rmse <- min(cv_model$evaluation_log$test_rmse_mean)
        best_iter_for_this_run <- cv_model$best_iteration
        
        # Guardar resultado detallado
        all_cv_results[[i]] <- data.table(
          eta = params$eta,
          max_depth = params$max_depth,
          nrounds_tested = nrounds_candidate,
          best_iteration = best_iter_for_this_run,
          test_rmse_mean = current_best_rmse,
          time_secs = as.numeric(difftime(
            cv_iter_end, cv_iter_start, units = "secs"
          ))
        )
        
        log_info(
          "    -> CV RMSE: {round(current_best_rmse, 5)} en iter {best_iter_for_this_run} ({round(all_cv_results[[i]]$time_secs, 1)}s)"
        )
        
        # Actualizar el mejor global
        if (current_best_rmse < best_cv_rmse) {
          best_cv_rmse <- current_best_rmse
          best_cv_params <- params
          best_cv_nrounds <- best_iter_for_this_run # Guardar la mejor iteración
        }
      } else {
        log_warn("    -> Falló la ejecución de xgb.cv para esta combinación.")
        all_cv_results[[i]] <- data.table(
          # Registrar el fallo
          eta = params$eta,
          max_depth = params$max_depth,
          nrounds_tested = nrounds_candidate,
          best_iteration = NA,
          test_rmse_mean = NA,
          time_secs = NA
        )
      }
      
      # Pausa breve para no sobrecargar CPU/logs
      Sys.sleep(0.1)
      
    } # --- Fin Bucle CV ---
    
    cv_end_global <- Sys.time()
    all_cv_results_dt <- rbindlist(all_cv_results)
    
    log_success(
      "Búsqueda CV completada en {round(difftime(cv_end_global, cv_start_global, units='mins'), 1)} mins"
    )
    
    if (is.infinite(best_cv_rmse)) {
      log_error(
        "❌ No se pudo encontrar una configuración válida en xgb.cv. SALTANDO entrenamiento final."
      )
      next
    }
    
    log_success("Mejor CV RMSE: {round(best_cv_rmse, 5)}")
    log_success("Mejores parámetros encontrados:")
    log_info(paste0("  ", names(best_cv_params), ": ", best_cv_params, collapse = "\n"))
    log_info("  Mejor nrounds (iteración): {best_cv_nrounds}")
    
    
    # --- Entrenar Modelo Final ---
    log_info("Entrenando modelo final con los mejores parámetros...")
    final_model_start_time <- Sys.time()
    final_model <- xgb.train(
      params = best_cv_params,
      data = dtrain,
      nrounds = best_cv_nrounds,
      # Usar el mejor número de iteraciones encontrado
      verbose = 1 # Mostrar progreso cada X iteraciones
    )
    final_model_end_time <- Sys.time()
    tiempo_modelo_final <- difftime(final_model_end_time, final_model_start_time, units = "mins")
    
    
    # --- Guardar Resultados ---
    modelos_xgb_nativo[[contam]] <- final_model # Guardar el modelo xgb.Booster
    
    metricas <- data.table(
      contaminante = contam,
      n_obs = nrow(datos_contam),
      nrounds_optimo = best_cv_nrounds,
      max_depth_optimo = best_cv_params$max_depth,
      eta_optimo = best_cv_params$eta,
      RMSE_cv = best_cv_rmse,
      # RMSE obtenido de xgb.cv
      # Rsquared y MAE no son calculados directamente por xgb.cv
      Rsquared_cv = NA_real_,
      MAE_cv = NA_real_,
      tiempo_train_final_mins = as.numeric(tiempo_modelo_final),
      tiempo_cv_total_mins = as.numeric(difftime(
        cv_end_global, cv_start_global, units = "mins"
      ))
    )
    metricas_xgb[[contam]] <- metricas
    
    log_success("✅ MODELO XGBOOST NATIVO ENTRENADO EXITOSAMENTE")
    log_info("Tiempo (entrenamiento final): {round(tiempo_modelo_final, 2)} minutos")
    
    # --- Importancia de Variables (xgboost nativo) ---
    tryCatch({
      importancia_matrix <- xgb.importance(model = final_model)
      log_info("\nTop 10 variables más importantes (xgboost nativo):")
      print(head(importancia_matrix, 10))
    }, error = function(e) {
      log_warn("No se pudo calcular la importancia de variables: {e$message}")
    })
    
    gc(verbose = FALSE)
    
  } # --- Fin Bucle Contaminantes ---
  
  
  # ==================== GUARDAR MODELOS Y RESULTADOS ====================
  log_info("\n============================================================")
  log_info("GUARDANDO MODELOS NATIVOS Y RESULTADOS")
  log_info("============================================================")
  
  if (!dir.exists("models")) {
    dir.create("models")
  }
  
  for (contam in names(modelos_xgb_nativo)) {
    nombre_limpio <- gsub("[^a-zA-Z0-9]", "_", contam)
    
    # Guardar modelo xgboost nativo (MUCHO más pequeño)
    archivo_xgb <- glue("models/xgboost_nativo_ica_{nombre_limpio}.model")
    xgb.save(modelos_xgb_nativo[[contam]], archivo_xgb)
    
    size_xgb <- file.size(archivo_xgb) / 1024^2
    log_info("✅ {contam}: Guardado modelo nativo ({round(size_xgb, 2)} MB)")
    
    # Opcional: Guardar también el objeto caret si lo hubiéramos generado
    # (En este caso, modelos_xgb_nativo contiene xgb.Booster, no objetos train)
  }
  
  # Guardar todos los modelos nativos juntos (como lista)
  saveRDS(modelos_xgb_nativo, "models/xgboost_nativo_ica_todos.rds")
  log_success("✅ Todos los modelos NATIVOS guardados: models/xgboost_nativo_ica_todos.rds")
  
  # Consolidar métricas
  if (length(metricas_xgb) > 0) {
    metricas_consolidadas <- rbindlist(metricas_xgb)
    saveRDS(metricas_consolidadas,
            "models/xgboost_nativo_ica_metricas.rds")
    
    log_info("\n============================================================")
    log_info("RESUMEN FINAL - MÉTRICAS XGBOOST NATIVO ICA")
    log_info("============================================================")
    print(metricas_consolidadas)
    
    log_info("\nPromedios globales (basados en CV):")
    log_info(
      "  RMSE CV medio: {round(metricas_consolidadas[, mean(RMSE_cv, na.rm=TRUE)], 4)} µg/m³"
    )
    # log_info("  R² medio: ...") # No disponible fácilmente desde xgb.cv
    # log_info("  MAE medio: ...") # No disponible fácilmente desde xgb.cv
    log_info(
      "  Tiempo CV total: {round(metricas_consolidadas[, sum(tiempo_cv_total_mins)], 2)} minutos"
    )
    log_info(
      "  Tiempo Train (final) total: {round(metricas_consolidadas[, sum(tiempo_train_final_mins)], 2)} minutos"
    )
    
  } else {
    log_warn("No se generaron métricas, posible fallo en todos los entrenamientos.")
  }
  
  log_success("\n🎉 ENTRENAMIENTO XGBOOST NATIVO ICA COMPLETADO EXITOSAMENTE")
  log_info("Modelos entrenados: {length(modelos_xgb_nativo)}/{length(contaminantes_ica)}")
  log_info(
    "Observaciones iniciales procesadas: {format(nrow(datos_muestra), big.mark=',')}"
  )
  log_info("Hardware utilizado: CPU (tree_method='hist')")
  
  
}, error = function(e) {
  log_error("Error fatal en el script principal: {e$message}")
  log_error("Stack trace: {paste(deparse(sys.calls()), collapse = '\n')}")
}, finally = {
  if (!is.null(con) && dbIsValid(con)) {
    dbDisconnect(con)
    log_info("\nConexión a BD cerrada.")
  }
})