# MODELO RANGER OPTIMIZADO PARA ICA (5 CONTAMINANTES)
# Objetivo: Entrenar modelos con datos reales AEMET usando ranger + muestreo estratificado
# Hardware: 16 cores CPU, 61GB RAM, RTX 4070Ti

library(dplyr)
library(caret)
library(ranger)  # Faster than randomForest
library(logger)
library(DBI)
library(RPostgres)
library(lubridate)
library(glue)

# ==================== CONFIGURACIÓN ====================
log_threshold(INFO)
log_appender(appender_tee("logs/modelo_ranger_ica.log"))
log_info("=== MODELO RANGER ICA - 5 CONTAMINANTES ===")

# Parámetros de muestreo
PORCENTAJE_MUESTRA <- 0.25  # 25% del dataset completo (~2.5M obs de 10M total)
SEED <- 42
set.seed(SEED)

# Cargar utilidades de expansión meteorológica
if(file.exists("R/utils_meteo_horario.R")) {
  source("R/utils_meteo_horario.R")
  log_info("✅ utils_meteo_horario.R cargado")
} else {
  log_warn("⚠️ utils_meteo_horario.R no encontrado, usando expansión básica")
}

# ==================== CONEXIÓN BD ====================
readRenviron('.Renviron')

con <- dbConnect(
  RPostgres::Postgres(),
  host = Sys.getenv("DB_HOST"),
  port = Sys.getenv("DB_PORT"),
  dbname = Sys.getenv("DB_NAME"),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)
log_success("✅ Conectado a PostgreSQL en {Sys.getenv('DB_HOST')}")

# ==================== QUERY DATOS REALES CON PROYECCIÓN UTM ====================
# Incluye AEMET real + 5 contaminantes ICA + Proyección UTM Zone 30N
query_ica <- glue("
  SELECT
    fm.fecha_hora,
    fm.valor_medido as valor_medio,
    de.\"LONGITUD\"::FLOAT as longitud,
    de.\"LATITUD\"::FLOAT as latitud,
    -- Proyección UTM Zone 30N (EPSG:25830) - coordenadas cartesianas en metros
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
    -- DATOS METEOROLÓGICOS REALES AEMET (diarios)
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

log_info("Cargando datos históricos 2015-2025 (5 contaminantes ICA)...")
inicio_carga <- Sys.time()
datos_raw <- dbGetQuery(con, query_ica)
dbDisconnect(con)

tiempo_carga <- difftime(Sys.time(), inicio_carga, units = "secs")
log_success("✅ Datos cargados: {format(nrow(datos_raw), big.mark=',')} observaciones en {round(tiempo_carga, 1)}s")
log_info("Período: {min(datos_raw$fecha)} a {max(datos_raw$fecha)}")

# Distribución por contaminante
dist_contaminantes <- datos_raw %>%
  count(contaminante, sort = TRUE)
log_info("\nDistribución de observaciones por contaminante:")
for(i in 1:nrow(dist_contaminantes)) {
  log_info("  {dist_contaminantes$contaminante[i]}: {format(dist_contaminantes$n[i], big.mark=',')}")
}

# ==================== EXPANSIÓN METEOROLÓGICA ====================
log_info("\nExpandiendo datos meteorológicos diarios a horarios...")

# Preparar datos meteorológicos diarios únicos
datos_meteo_diarios <- datos_raw %>%
  select(fecha, temp_media_c, temp_maxima_c, temp_minima_c,
         precipitacion_mm, vel_viento_media_ms, dir_viento_grados,
         presion_maxima_hpa, presion_minima_hpa,
         humedad_media_pct, humedad_maxima_pct, humedad_minima_pct) %>%
  distinct(fecha, .keep_all = TRUE)

log_info("Fechas únicas con datos meteorológicos: {nrow(datos_meteo_diarios)}")

# Expandir a horarios (24 horas por cada fecha)
# USAR EXPANSIÓN SINUSOIDAL (más precisa que lineal según validación)
if(exists("expandir_meteo_sinusoidal")) {
  datos_meteo_horarios <- expandir_meteo_sinusoidal(datos_meteo_diarios)
  log_success("✅ Expansión meteorológica SINUSOIDAL completada: {format(nrow(datos_meteo_horarios), big.mark=',')} registros horarios")
} else if(exists("expandir_meteo_lineal")) {
  log_warn("⚠️ Usando expansión lineal (sinusoidal no disponible)")
  datos_meteo_horarios <- expandir_meteo_lineal(datos_meteo_diarios)
  log_success("✅ Expansión meteorológica lineal completada: {format(nrow(datos_meteo_horarios), big.mark=',')} registros horarios")
} else {
  log_error("❌ Funciones de expansión meteorológica no encontradas")
  stop("Expansión meteorológica fallida")
}

# ==================== JOIN METEOROLOGÍA HORARIA ====================
log_info("Uniendo datos de contaminantes con meteorología horaria...")

# La función expandir_meteo_lineal devuelve nombres diferentes
# Necesitamos renombrar para coincidir con nombres esperados
datos_meteo_horarios_renamed <- datos_meteo_horarios %>%
  rename(
    temp_media_c_horario = temp_c,
    humedad_media_pct_horario = humedad_pct,
    presion_media_hpa = presion_hpa,
    vel_viento_media_ms_horario = vel_viento_ms,
    precipitacion_mm_horario = precipitacion_mm
  ) %>%
  select(fecha, hora, temp_media_c_horario, humedad_media_pct_horario,
         presion_media_hpa, vel_viento_media_ms_horario, precipitacion_mm_horario)

datos_completos <- datos_raw %>%
  left_join(
    datos_meteo_horarios_renamed,
    by = c("fecha", "hora"),
    relationship = "many-to-one"
  ) %>%
  # Usar datos horarios expandidos cuando estén disponibles
  mutate(
    temp_media_c = coalesce(temp_media_c_horario, temp_media_c),
    humedad_media_pct = coalesce(humedad_media_pct_horario, humedad_media_pct),
    vel_viento_media_ms = coalesce(vel_viento_media_ms_horario, vel_viento_media_ms),
    precipitacion_mm = coalesce(precipitacion_mm_horario, precipitacion_mm),
    # dir_viento_grados se mantiene del diario (no se expande)
    # presion_maxima/minima se mantienen del diario
    # temp_maxima/minima se mantienen del diario
  ) %>%
  select(-temp_media_c_horario, -humedad_media_pct_horario,
         -vel_viento_media_ms_horario, -precipitacion_mm_horario) %>%
  filter(!is.na(temp_media_c))  # Solo registros con meteorología completa

log_info("Datos tras JOIN: {format(nrow(datos_completos), big.mark=',')} observaciones")

# ==================== MUESTREO ESTRATIFICADO ====================
log_info("\nAplicando muestreo estratificado ({PORCENTAJE_MUESTRA*100}%)...")
log_info("Estratificación por: contaminante + año + mes")

datos_muestra <- datos_completos %>%
  group_by(contaminante, año, mes) %>%
  slice_sample(prop = PORCENTAJE_MUESTRA) %>%
  ungroup()

log_success("✅ Muestra generada: {format(nrow(datos_muestra), big.mark=',')} observaciones")

# Verificar distribución post-muestreo
dist_muestra <- datos_muestra %>%
  count(contaminante, sort = TRUE) %>%
  mutate(porcentaje = n / sum(n) * 100)

log_info("\nDistribución muestra por contaminante:")
for(i in 1:nrow(dist_muestra)) {
  log_info("  {dist_muestra$contaminante[i]}: {format(dist_muestra$n[i], big.mark=',')} ({round(dist_muestra$porcentaje[i], 1)}%)")
}

# ==================== PREPARACIÓN DATOS ML CON PROYECCIÓN UTM ====================
log_info("\nPreparando datos para modelado con proyección UTM...")

# Centro de Madrid en UTM (Puerta del Sol aprox.)
MADRID_CENTRO_UTM_X <- 440000  # metros
MADRID_CENTRO_UTM_Y <- 4474000  # metros

datos_ml <- datos_muestra %>%
  mutate(
    # VARIABLES ESPACIALES UTM (en metros, proyección cartesiana)
    utm_x = as.numeric(utm_x),
    utm_y = as.numeric(utm_y),

    # Distancia euclidea al centro de Madrid (en km)
    dist_centro_madrid = sqrt((utm_x - MADRID_CENTRO_UTM_X)^2 +
                              (utm_y - MADRID_CENTRO_UTM_Y)^2) / 1000,

    # Componentes direccionales desde el centro (normalizadas)
    utm_x_norm = (utm_x - MADRID_CENTRO_UTM_X) / 10000,  # En decenas de km
    utm_y_norm = (utm_y - MADRID_CENTRO_UTM_Y) / 10000,

    # Variables temporales cíclicas
    sin_hora = sin(2 * pi * hora / 24),
    cos_hora = cos(2 * pi * hora / 24),
    sin_dia_año = sin(2 * pi * dia_año / 365),
    cos_dia_año = cos(2 * pi * dia_año / 365),
    fin_semana = ifelse(dia_semana %in% c(0, 6), 1, 0),  # Domingo=0, Sábado=6

    # Variables meteorológicas derivadas
    temp_sq = temp_media_c^2,
    temp_hum_ratio = temp_media_c / (humedad_media_pct + 1),

    # Variables condicionales (solo si existen las columnas)
    presion_diff = ifelse("presion_maxima_hpa" %in% names(.) & "presion_minima_hpa" %in% names(.),
                          presion_maxima_hpa - presion_minima_hpa, NA_real_),
    humedad_diff = ifelse("humedad_maxima_pct" %in% names(.) & "humedad_minima_pct" %in% names(.),
                          humedad_maxima_pct - humedad_minima_pct, NA_real_),
    temp_range = ifelse("temp_maxima_c" %in% names(.) & "temp_minima_c" %in% names(.),
                        temp_maxima_c - temp_minima_c, NA_real_),

    # Componentes del viento (usar dirección diaria si existe)
    dir_viento = ifelse("dir_viento_grados" %in% names(.), dir_viento_grados, 180),
    viento_x = vel_viento_media_ms * cos(dir_viento * pi / 180),
    viento_y = vel_viento_media_ms * sin(dir_viento * pi / 180)
  ) %>%
  select(
    # Variable objetivo
    valor_medio,
    # Identificadores para split por contaminante
    contaminante,
    # Predictores temporales
    hora, dia_semana, mes, dia_año, sin_hora, cos_hora,
    sin_dia_año, cos_dia_año, fin_semana,
    # Predictores meteorológicos básicos
    temp_media_c, temp_sq, temp_hum_ratio,
    precipitacion_mm, vel_viento_media_ms,
    viento_x, viento_y,
    # Predictores espaciales UTM (cartesianos en metros)
    utm_x, utm_y, utm_x_norm, utm_y_norm, dist_centro_madrid,
    # Predictores opcionales (si existen)
    any_of(c("temp_range", "presion_diff", "humedad_diff", "dir_viento",
             "presion_maxima_hpa", "presion_minima_hpa", "presion_media_hpa",
             "humedad_maxima_pct", "humedad_minima_pct"))
  ) %>%
  filter(complete.cases(.)) %>%
  # CRÍTICO: Eliminar variables con un solo valor único
  select(where(~n_distinct(.) > 1))

log_success("✅ Datos ML preparados: {format(nrow(datos_ml), big.mark=',')} obs, {ncol(datos_ml)-2} predictores")
log_info("Estadísticas generales:")
log_info("  Media concentración: {round(mean(datos_ml$valor_medio), 2)} µg/m³")
log_info("  Rango: {round(min(datos_ml$valor_medio), 2)} - {round(max(datos_ml$valor_medio), 2)} µg/m³")

# ==================== ENTRENAMIENTO POR CONTAMINANTE ====================
log_info("\n============================================================")
log_info("INICIANDO ENTRENAMIENTO RANGER - 5 CONTAMINANTES ICA")
log_info("============================================================")

# Control de entrenamiento
control <- trainControl(
  method = "cv",
  number = 5,  # 5-fold CV
  verboseIter = TRUE,
  allowParallel = TRUE  # ranger ya paraleliza internamente
)

# Grid de hiperparámetros para ranger
tune_grid <- expand.grid(
  mtry = c(5, 8, 12),              # Número de variables por split
  splitrule = c("variance", "extratrees"),  # Regla de división
  min.node.size = c(5, 10)         # Tamaño mínimo de nodo
)

log_info("Grid de hiperparámetros: {nrow(tune_grid)} combinaciones")
log_info("  mtry: {paste(unique(tune_grid$mtry), collapse=', ')}")
log_info("  splitrule: {paste(unique(tune_grid$splitrule), collapse=', ')}")
log_info("  min.node.size: {paste(unique(tune_grid$min.node.size), collapse=', ')}")

# Almacenar resultados
modelos_ica <- list()
metricas_ica <- list()

# Contaminantes ICA
contaminantes_ica <- c(
  "Dióxido de Nitrógeno",
  "Partículas < 10 µm",
  "Partículas < 2.5 µm",
  "Ozono",
  "Dióxido de Azufre"
)

# Entrenar cada contaminante
for(contam in contaminantes_ica) {

  log_info("\n============================================================")
  log_info("CONTAMINANTE: {contam}")
  log_info("============================================================")

  # Filtrar datos del contaminante
  datos_contam <- datos_ml %>%
    filter(contaminante == contam) %>%
    select(-contaminante)  # Remover identificador

  log_info("Observaciones: {format(nrow(datos_contam), big.mark=',')}")
  log_info("Media: {round(mean(datos_contam$valor_medio), 2)} µg/m³")
  log_info("Desviación estándar: {round(sd(datos_contam$valor_medio), 2)} µg/m³")

  # Verificar que hay suficientes datos
  if(nrow(datos_contam) < 1000) {
    log_warn("⚠️ Menos de 1000 observaciones, SALTANDO contaminante")
    next
  }

  # Entrenar modelo ranger
  log_info("Entrenando modelo ranger...")
  inicio_modelo <- Sys.time()

  tryCatch({
    modelo <- train(
      valor_medio ~ .,
      data = datos_contam,
      method = "ranger",
      trControl = control,
      tuneGrid = tune_grid,
      num.trees = 200,        # 200 árboles (mejor precisión vs 100)
      importance = "impurity", # Importancia de variables
      num.threads = 16        # Usar todos los cores
    )

    tiempo_modelo <- difftime(Sys.time(), inicio_modelo, units = "mins")

    # Guardar modelo y métricas
    modelos_ica[[contam]] <- modelo

    metricas <- data.frame(
      contaminante = contam,
      n_obs = nrow(datos_contam),
      mtry_optimo = modelo$bestTune$mtry,
      splitrule_optimo = modelo$bestTune$splitrule,
      min_node_size_optimo = modelo$bestTune$min.node.size,
      RMSE = min(modelo$results$RMSE),
      Rsquared = max(modelo$results$Rsquared, na.rm = TRUE),
      MAE = min(modelo$results$MAE, na.rm = TRUE),
      tiempo_mins = as.numeric(tiempo_modelo)
    )

    metricas_ica[[contam]] <- metricas

    log_success("✅ MODELO ENTRENADO EXITOSAMENTE")
    log_info("Tiempo: {round(tiempo_modelo, 2)} minutos")
    log_info("Mejor configuración:")
    log_info("  mtry: {modelo$bestTune$mtry}")
    log_info("  splitrule: {modelo$bestTune$splitrule}")
    log_info("  min.node.size: {modelo$bestTune$min.node.size}")
    log_info("Métricas:")
    log_info("  RMSE: {round(metricas$RMSE, 4)} µg/m³")
    log_info("  R²: {round(metricas$Rsquared, 4)}")
    log_info("  MAE: {round(metricas$MAE, 4)} µg/m³")

    # Top 10 variables importantes
    importancia <- varImp(modelo, scale = FALSE)
    top_vars <- head(rownames(importancia$importance)[
      order(importancia$importance$Overall, decreasing = TRUE)], 10)

    log_info("\nTop 10 variables más importantes:")
    for(i in seq_along(top_vars)) {
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
if(!dir.exists("models")) dir.create("models")

# Guardar modelos individuales
for(contam in names(modelos_ica)) {
  nombre_limpio <- gsub("[^a-zA-Z0-9]", "_", contam)
  archivo <- glue("models/ranger_ica_{nombre_limpio}.rds")
  saveRDS(modelos_ica[[contam]], archivo)
  log_info("✅ Guardado: {archivo}")
}

# Guardar todos los modelos juntos
saveRDS(modelos_ica, "models/ranger_ica_todos.rds")
log_success("✅ Todos los modelos guardados: models/ranger_ica_todos.rds")

# Consolidar métricas
metricas_consolidadas <- bind_rows(metricas_ica)
saveRDS(metricas_consolidadas, "models/ranger_ica_metricas.rds")

log_info("\n============================================================")
log_info("RESUMEN FINAL - MÉTRICAS ICA")
log_info("============================================================")

print(metricas_consolidadas)

# Calcular promedios
log_info("\nPromedios globales:")
log_info("  RMSE medio: {round(mean(metricas_consolidadas$RMSE), 4)} µg/m³")
log_info("  R² medio: {round(mean(metricas_consolidadas$Rsquared), 4)}")
log_info("  MAE medio: {round(mean(metricas_consolidadas$MAE), 4)} µg/m³")
log_info("  Tiempo total: {round(sum(metricas_consolidadas$tiempo_mins), 2)} minutos")

log_success("\n🎉 ENTRENAMIENTO ICA COMPLETADO EXITOSAMENTE")
log_info("Modelos entrenados: {length(modelos_ica)}/5")
log_info("Observaciones procesadas: {format(nrow(datos_ml), big.mark=',')}")
log_info("Hardware utilizado: 16 cores CPU, ranger optimizado")
