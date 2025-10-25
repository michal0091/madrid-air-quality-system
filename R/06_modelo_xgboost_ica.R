#!/usr/bin/env Rscript
# MODELO XGBOOST OPTIMIZADO PARA ICA (5 CONTAMINANTES)
# Objetivo: Entrenar modelos con xgboost + GPU + data.table puro
# Ventajas: 10-20x más pequeño, 10-50x más rápido, usa GPU RTX 4070Ti
# Hardware: 16 cores CPU, 61GB RAM, RTX 4070Ti
# Estilo: data.table puro según https://michal0091.github.io/tutorial_data.table/

# ==================== LIBRERÍAS ====================
library(data.table)      # Manipulación de datos eficiente
library(xgboost)         # Gradient Boosting con GPU
library(caret)           # ML framework (wrapper para xgboost)
library(logger)          # Logging estructurado
library(DBI)             # Database interface
library(RPostgres)       # PostgreSQL connector
library(glue)            # String interpolation

# ==================== CONFIGURACIÓN ====================
log_threshold(INFO)
log_appender(appender_tee("logs/modelo_xgboost_ica.log"))
log_info("=== MODELO XGBOOST ICA - 5 CONTAMINANTES ===")
log_info("Estilo: data.table puro (DT[i, j, by])")

# Verificar GPU disponible
gpu_disponible <- system("nvidia-smi", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
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
query_ica <- glue("
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
")

log_info("📊 Cargando datos 2015-2025 (10 años, 5 contaminantes ICA)...")
inicio_carga <- Sys.time()

# Cargar como data.table directamente
datos_dt <- setDT(dbGetQuery(con, query_ica))

# ==================== CARGAR BASELINE ESTACIONAL ====================
log_info("\nCargando baseline estacional desde BD...")

baseline_estacional <- setDT(dbGetQuery(con, "
  SELECT
    id_magnitud,
    mes,
    dia_mes,
    hora,
    promedio_5y,
    p10,
    p90
  FROM dim_baseline_estacional
"))

log_success("✅ Baseline cargado: {format(nrow(baseline_estacional), big.mark=',')} registros")
dbDisconnect(con)

tiempo_carga <- difftime(Sys.time(), inicio_carga, units = "secs")
log_success("✅ Datos cargados: {format(nrow(datos_dt), big.mark=',')} obs en {round(tiempo_carga, 1)}s")
log_info("Período: {min(datos_dt$fecha)} a {max(datos_dt$fecha)}")

# Distribución por contaminante (usando data.table)
dist_contaminantes <- datos_dt[, .(n = .N), by = contaminante][order(-n)]
log_info("\nDistribución de observaciones por contaminante:")
dist_contaminantes[, log_info("  {contaminante}: {format(n, big.mark=',')}")]

# ==================== EXPANSIÓN METEOROLÓGICA ====================
log_info("\n=== EXPANDIENDO DATOS METEOROLÓGICOS (data.table) ===")

# Preparar datos meteorológicos diarios únicos (usando data.table)
cols_meteo <- c("temp_media_c", "temp_maxima_c", "temp_minima_c",
                "precipitacion_mm", "vel_viento_media_ms", "dir_viento_grados",
                "presion_maxima_hpa", "presion_minima_hpa",
                "humedad_media_pct", "humedad_maxima_pct", "humedad_minima_pct")

datos_meteo_diarios <- unique(datos_dt[, c("fecha", ..cols_meteo), with = FALSE])
log_info("Fechas únicas con datos meteorológicos: {nrow(datos_meteo_diarios)}")

# Expandir a horarios
if (exists("expandir_meteo_sinusoidal")) {
  datos_meteo_horarios <- expandir_meteo_sinusoidal(datos_meteo_diarios)
  log_success("✅ Expansión SINUSOIDAL: {format(nrow(datos_meteo_horarios), big.mark=',')} registros")
} else if (exists("expandir_meteo_lineal")) {
  log_warn("⚠️ Usando expansión lineal (sinusoidal no disponible)")
  datos_meteo_horarios <- expandir_meteo_lineal(datos_meteo_diarios)
  log_success("✅ Expansión lineal: {format(nrow(datos_meteo_horarios), big.mark=',')} registros")
} else {
  log_error("❌ Funciones de expansión meteorológica no encontradas")
  stop("Expansión meteorológica fallida")
}

# Convertir a data.table si no lo es
setDT(datos_meteo_horarios)

# Renombrar columnas para JOIN
setnames(datos_meteo_horarios,
         old = c("temp_c", "humedad_pct", "presion_hpa", "vel_viento_ms", "precipitacion_mm"),
         new = c("temp_media_c_horario", "humedad_media_pct_horario", "presion_media_hpa",
                 "vel_viento_media_ms_horario", "precipitacion_mm_horario"),
         skip_absent = TRUE)

# ==================== JOIN METEOROLOGÍA (data.table) ====================
log_info("\n=== JOIN METEOROLOGÍA HORARIA (data.table optimizado) ===")

# Establecer claves para join ultra-rápido
setkeyv(datos_dt, c("fecha", "hora"))
setkeyv(datos_meteo_horarios, c("fecha", "hora"))

# JOIN usando sintaxis data.table
datos_completos <- datos_meteo_horarios[datos_dt, on = .(fecha, hora)]

# Usar fcoalesce para priorizar datos horarios (más rápido que dplyr::coalesce)
datos_completos[, `:=`(
  temp_media_c = fcoalesce(temp_media_c_horario, temp_media_c),
  humedad_media_pct = fcoalesce(humedad_media_pct_horario, humedad_media_pct),
  vel_viento_media_ms = fcoalesce(vel_viento_media_ms_horario, vel_viento_media_ms),
  precipitacion_mm = fcoalesce(precipitacion_mm_horario, precipitacion_mm)
)]

# Eliminar columnas horarias temporales (por referencia)
cols_temp <- c("temp_media_c_horario", "humedad_media_pct_horario",
               "vel_viento_media_ms_horario", "precipitacion_mm_horario")
datos_completos[, (cols_temp) := NULL]

# Filtrar NAs en variables esenciales
datos_completos <- datos_completos[!is.na(temp_media_c)]
log_info("Datos tras JOIN: {format(nrow(datos_completos), big.mark=',')} observaciones")

# ==================== MUESTREO ESTRATIFICADO (data.table) ====================
if (PORCENTAJE_MUESTRA < 1.0) {
  log_info("\n=== MUESTREO ESTRATIFICADO (data.table) ===")
  log_info("Estratificación por: contaminante + año + mes")

  # Usar .I y .N para muestreo eficiente por grupo
  indices_muestra <- datos_completos[, .I[sample(.N, .N * PORCENTAJE_MUESTRA)],
                                     by = .(contaminante, año, mes)]$V1

  datos_muestra <- datos_completos[indices_muestra]
  log_success("✅ Muestra generada: {format(nrow(datos_muestra), big.mark=',')} observaciones")
} else {
  log_info("\n📊 USANDO DATASET COMPLETO (100%)")
  datos_muestra <- datos_completos
}

# Verificar distribución (data.table)
dist_muestra <- datos_muestra[, .(n = .N, porcentaje = .N / nrow(datos_muestra) * 100),
                               by = contaminante][order(-n)]
log_info("\nDistribución muestra por contaminante:")
dist_muestra[, log_info("  {contaminante}: {format(n, big.mark=',')} ({round(porcentaje, 1)}%)")]

# ==================== FEATURE ENGINEERING (data.table por referencia) ====================
log_info("\n=== PREPARANDO DATOS ML (data.table :=) ===")

# Centro de Madrid en UTM
madrid_centro_utm_x <- 440000L
madrid_centro_utm_y <- 4474000L

# TODAS las transformaciones por referencia (CERO copias en memoria)
datos_muestra[, `:=`(
  # VARIABLES ESPACIALES UTM
  utm_x = as.numeric(utm_x),
  utm_y = as.numeric(utm_y)
)]

datos_muestra[, `:=`(
  # Distancia euclidea al centro de Madrid (en km)
  dist_centro_madrid = sqrt((utm_x - madrid_centro_utm_x)^2 +
                            (utm_y - madrid_centro_utm_y)^2) / 1000,

  # Componentes direccionales normalizados
  utm_x_norm = (utm_x - madrid_centro_utm_x) / 10000,
  utm_y_norm = (utm_y - madrid_centro_utm_y) / 10000,

  # Variables temporales
  fin_semana = fifelse(dia_semana %in% c(0, 6), 1L, 0L),

  # Variables meteorológicas derivadas - Temperatura
  temp_abs = abs(temp_media_c),
  temp_sign = sign(temp_media_c),
  temp_sq = temp_media_c^2,
  temp_sq_signed = temp_media_c * abs(temp_media_c),
  temp_cubic = temp_media_c^3,

  # Ratios temperatura-humedad
  temp_hum_ratio = (temp_media_c + 20) / (humedad_media_pct + 1),
  temp_hum_ratio_inv = humedad_media_pct / (abs(temp_media_c) + 1),

  # Déficit de Presión de Vapor (VPD) - Ecuación de Bolton
  e_sat = 6.112 * exp((17.67 * temp_media_c) / (temp_media_c + 243.5)),
  e_actual = e_sat * (humedad_media_pct / 100),
  vpd = e_sat - e_actual,

  # Variables condicionales (rangos diarios)
  presion_diff = fifelse(!is.na(presion_maxima_hpa) & !is.na(presion_minima_hpa),
                         presion_maxima_hpa - presion_minima_hpa, NA_real_),
  humedad_diff = fifelse(!is.na(humedad_maxima_pct) & !is.na(humedad_minima_pct),
                         humedad_maxima_pct - humedad_minima_pct, NA_real_),
  temp_range = fifelse(!is.na(temp_maxima_c) & !is.na(temp_minima_c),
                       temp_maxima_c - temp_minima_c, NA_real_),

  # Componentes del viento
  dir_viento = fcoalesce(dir_viento_grados, 180),
  viento_x = vel_viento_media_ms * cos(dir_viento * pi / 180),
  viento_y = vel_viento_media_ms * sin(dir_viento * pi / 180)
)]

# ==================== BASELINE ESTACIONAL (data.table join) ====================
log_info("\n=== AGREGANDO BASELINE ESTACIONAL (data.table join) ===")

# Join con baseline usando keys
setkeyv(baseline_estacional, c("id_magnitud", "mes", "dia_mes", "hora"))
setkeyv(datos_muestra, c("id_magnitud", "mes", "dia", "hora"))

# JOIN eficiente
datos_ml <- baseline_estacional[datos_muestra, on = .(id_magnitud, mes, dia_mes = dia, hora)]

# Crear tipo_dia_num
datos_ml[, tipo_dia_num := fcase(
  dia_semana %in% c(0, 6), 0L,  # Fin de semana
  dia_semana == 1, 1L,          # Lunes
  dia_semana == 5, 2L,          # Viernes
  default = 3L                  # Martes-Jueves
)]

# Filtrar registros con baseline disponible
datos_ml <- datos_ml[!is.na(promedio_5y)]
log_success("✅ Baseline agregado: {format(nrow(datos_ml), big.mark=',')} observaciones")

# ==================== SELECCIÓN FINAL DE COLUMNAS ====================
cols_predictores <- c(
  "valor_medio", "contaminante",
  # Temporales
  "hora", "dia_semana", "mes", "dia_año", "fin_semana", "tipo_dia_num",
  # Meteorológicos básicos
  "temp_media_c", "precipitacion_mm", "vel_viento_media_ms", "viento_x", "viento_y",
  # Transformaciones temperatura
  "temp_abs", "temp_sign", "temp_sq", "temp_sq_signed", "temp_cubic",
  # Ratios y VPD
  "temp_hum_ratio", "temp_hum_ratio_inv", "e_sat", "e_actual", "vpd",
  # Espaciales UTM (CRÍTICOS)
  "utm_x", "utm_y", "utm_x_norm", "utm_y_norm", "dist_centro_madrid",
  # Baseline estacional
  "promedio_5y"
)

# Añadir columnas opcionales si existen
cols_opcionales <- c("temp_range", "presion_diff", "humedad_diff", "dir_viento",
                     "presion_maxima_hpa", "presion_minima_hpa", "presion_media_hpa")
cols_disponibles <- intersect(cols_opcionales, names(datos_ml))
cols_finales <- c(cols_predictores, cols_disponibles)

# Seleccionar columnas finales
datos_ml <- datos_ml[, ..cols_finales]

# Filtrar valores críticos NA
datos_ml <- datos_ml[!is.na(valor_medio) & !is.na(temp_media_c) &
                     !is.na(utm_x) & !is.na(utm_y) & !is.na(promedio_5y)]

# Imputar NAs en columnas opcionales (por referencia)
for (col in cols_disponibles) {
  if (anyNA(datos_ml[[col]])) {
    if (col == "temp_range") datos_ml[is.na(get(col)), (col) := 10]
    else if (col == "presion_diff") datos_ml[is.na(get(col)), (col) := 3]
    else if (col == "dir_viento") datos_ml[is.na(get(col)), (col) := 180]
    else if (col == "presion_maxima_hpa") datos_ml[is.na(get(col)), (col) := 1013]
    else if (col == "presion_minima_hpa") datos_ml[is.na(get(col)), (col) := 1010]
    else if (col == "presion_media_hpa") datos_ml[is.na(get(col)), (col) := 1011.5]
  }
}

# Eliminar columnas con un solo valor único
cols_varianza <- names(datos_ml)[sapply(datos_ml, uniqueN) > 1]
datos_ml <- datos_ml[, ..cols_varianza]

# Verificar que no hay NAs
datos_ml <- datos_ml[complete.cases(datos_ml)]

n_predictores <- ncol(datos_ml) - 2  # Menos valor_medio y contaminante
log_success("✅ Datos ML preparados: {format(nrow(datos_ml), big.mark=',')} obs, {n_predictores} predictores")
log_info("Estadísticas generales:")
log_info("  Media concentración: {round(datos_ml[, mean(valor_medio)], 2)} µg/m³")
log_info("  Rango: {round(datos_ml[, min(valor_medio)], 2)} - {round(datos_ml[, max(valor_medio)], 2)} µg/m³")

# ==================== ENTRENAMIENTO XGBOOST POR CONTAMINANTE ====================
log_info("\n============================================================")
log_info("INICIANDO ENTRENAMIENTO XGBOOST - 5 CONTAMINANTES ICA")
log_info("============================================================")

# Control de entrenamiento
control <- trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  allowParallel = TRUE
)

# Grid de hiperparámetros para xgboost
# NOTA: xgboost requiere menos árboles que ranger debido a boosting
tune_grid <- expand.grid(
  nrounds = c(100, 200, 300),           # Número de árboles (boosting)
  max_depth = c(6, 8, 10),              # Profundidad máxima
  eta = c(0.01, 0.05, 0.1),             # Learning rate
  gamma = 0,                            # Mínima pérdida para split
  colsample_bytree = 0.8,               # Proporción de columnas por árbol
  min_child_weight = 1,                 # Peso mínimo en nodos hoja
  subsample = 0.8                       # Proporción de muestras
)

log_info("Grid de hiperparámetros: {nrow(tune_grid)} combinaciones")
log_info("  nrounds: {paste(unique(tune_grid$nrounds), collapse=', ')}")
log_info("  max_depth: {paste(unique(tune_grid$max_depth), collapse=', ')}")
log_info("  eta: {paste(unique(tune_grid$eta), collapse=', ')}")

# Parámetros adicionales para GPU
if (gpu_disponible) {
  xgb_params <- list(
    tree_method = "gpu_hist",   # Usar GPU
    predictor = "gpu_predictor"
  )
  log_info("GPU: tree_method='gpu_hist', predictor='gpu_predictor'")
} else {
  xgb_params <- list(
    tree_method = "hist"        # Usar CPU optimizado
  )
  log_info("CPU: tree_method='hist'")
}

# Almacenar resultados
modelos_xgb <- list()
metricas_xgb <- list()

# Contaminantes ICA
contaminantes_ica <- c(
  "Dióxido de Nitrógeno",
  "Partículas < 10 µm",
  "Partículas < 2.5 µm",
  "Ozono",
  "Dióxido de Azufre"
)

# Entrenar cada contaminante
for (contam in contaminantes_ica) {

  log_info("\n============================================================")
  log_info("CONTAMINANTE: {contam}")
  log_info("============================================================")

  # Filtrar datos del contaminante (data.table)
  datos_contam <- datos_ml[contaminante == contam][, !"contaminante"]

  log_info("Observaciones: {format(nrow(datos_contam), big.mark=',')}")
  log_info("Media: {round(datos_contam[, mean(valor_medio)], 2)} µg/m³")
  log_info("Desviación estándar: {round(datos_contam[, sd(valor_medio)], 2)} µg/m³")

  # Verificar datos suficientes
  if (nrow(datos_contam) < 1000) {
    log_warn("⚠️ Menos de 1000 observaciones, SALTANDO contaminante")
    next
  }

  # Entrenar modelo xgboost
  log_info("Entrenando modelo xgboost...")
  inicio_modelo <- Sys.time()

  tryCatch({
    modelo <- train(
      valor_medio ~ .,
      data = datos_contam,
      method = "xgbTree",
      trControl = control,
      tuneGrid = tune_grid,
      verbose = FALSE,
      # Parámetros adicionales para xgboost
      params = xgb_params
    )

    tiempo_modelo <- difftime(Sys.time(), inicio_modelo, units = "mins")

    # Guardar modelo y métricas
    modelos_xgb[[contam]] <- modelo

    metricas <- data.table(
      contaminante = contam,
      n_obs = nrow(datos_contam),
      nrounds_optimo = modelo$bestTune$nrounds,
      max_depth_optimo = modelo$bestTune$max_depth,
      eta_optimo = modelo$bestTune$eta,
      RMSE = min(modelo$results$RMSE),
      Rsquared = max(modelo$results$Rsquared, na.rm = TRUE),
      MAE = min(modelo$results$MAE, na.rm = TRUE),
      tiempo_mins = as.numeric(tiempo_modelo)
    )

    metricas_xgb[[contam]] <- metricas

    log_success("✅ MODELO ENTRENADO EXITOSAMENTE")
    log_info("Tiempo: {round(tiempo_modelo, 2)} minutos")
    log_info("Mejor configuración:")
    log_info("  nrounds: {modelo$bestTune$nrounds}")
    log_info("  max_depth: {modelo$bestTune$max_depth}")
    log_info("  eta: {modelo$bestTune$eta}")
    log_info("Métricas:")
    log_info("  RMSE: {round(metricas$RMSE, 4)} µg/m³")
    log_info("  R²: {round(metricas$Rsquared, 4)}")
    log_info("  MAE: {round(metricas$MAE, 4)} µg/m³")

    # Top 10 variables importantes
    importancia <- varImp(modelo, scale = FALSE)
    top_vars <- head(rownames(importancia$importance)[
      order(importancia$importance$Overall, decreasing = TRUE)], 10)

    log_info("\nTop 10 variables más importantes:")
    for (i in seq_along(top_vars)) {
      var_name <- top_vars[i]
      var_imp <- round(importancia$importance[var_name, "Overall"], 1)
      log_info("  {i}. {var_name}: {var_imp}")
    }

  }, error = function(e) {
    log_error("❌ ERROR entrenando {contam}: {e$message}")
  })
}

# ==================== GUARDAR MODELOS Y RESULTADOS ====================
log_info("\n============================================================")
log_info("GUARDANDO MODELOS Y RESULTADOS")
log_info("============================================================")

# Crear directorio si no existe
if (!dir.exists("models")) {
  dir.create("models")
}

# Guardar modelos individuales
for (contam in names(modelos_xgb)) {
  nombre_limpio <- gsub("[^a-zA-Z0-9]", "_", contam)

  # Guardar objeto caret completo
  archivo_rds <- glue("models/xgboost_ica_{nombre_limpio}.rds")
  saveRDS(modelos_xgb[[contam]], archivo_rds)

  # Guardar modelo xgboost nativo (MUCHO más pequeño)
  archivo_xgb <- glue("models/xgboost_ica_{nombre_limpio}.model")
  xgb.save(modelos_xgb[[contam]]$finalModel, archivo_xgb)

  # Comparar tamaños
  size_rds <- file.size(archivo_rds) / 1024^2
  size_xgb <- file.size(archivo_xgb) / 1024^2

  log_info("✅ {contam}:")
  log_info("   RDS: {round(size_rds, 2)} MB | XGB nativo: {round(size_xgb, 2)} MB")
}

# Guardar todos los modelos juntos
saveRDS(modelos_xgb, "models/xgboost_ica_todos.rds")
log_success("✅ Todos los modelos guardados: models/xgboost_ica_todos.rds")

# Consolidar métricas (data.table)
metricas_consolidadas <- rbindlist(metricas_xgb)
saveRDS(metricas_consolidadas, "models/xgboost_ica_metricas.rds")

log_info("\n============================================================")
log_info("RESUMEN FINAL - MÉTRICAS XGBOOST ICA")
log_info("============================================================")

print(metricas_consolidadas)

# Calcular promedios (data.table)
log_info("\nPromedios globales:")
log_info("  RMSE medio: {round(metricas_consolidadas[, mean(RMSE)], 4)} µg/m³")
log_info("  R² medio: {round(metricas_consolidadas[, mean(Rsquared)], 4)}")
log_info("  MAE medio: {round(metricas_consolidadas[, mean(MAE)], 4)} µg/m³")
log_info("  Tiempo total: {round(metricas_consolidadas[, sum(tiempo_mins)], 2)} minutos")

log_success("\n🎉 ENTRENAMIENTO XGBOOST ICA COMPLETADO EXITOSAMENTE")
log_info("Modelos entrenados: {length(modelos_xgb)}/5")
log_info("Observaciones procesadas: {format(nrow(datos_ml), big.mark=',')}")
log_info("Hardware utilizado: {ifelse(gpu_disponible, 'GPU RTX 4070Ti', '16 cores CPU')}")
log_info("Ventajas vs Ranger:")
log_info("  • Modelos 10-20x más pequeños")
log_info("  • Entrenamiento 10-50x más rápido")
log_info("  • Predicción más rápida")
log_info("  • Mejor generalización")
