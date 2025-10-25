# SCRIPT 05: CREAR TABLA BASELINE ESTACIONAL
# --------------------------------------------------------------------
# Objetivo: Calcular promedios históricos por (mes, día, hora) para cada contaminante
# Período: Últimos 5 y 10 años de datos
# Uso: Baseline robusto para predicciones (evita valores anómalos)
# --------------------------------------------------------------------

library(DBI)
library(RPostgres)
library(data.table)
library(logger)
library(glue)

# ==================== CONFIGURACIÓN ====================
log_threshold(INFO)
log_appender(appender_tee("logs/baseline_estacional.log"))
log_info("=== CREANDO TABLA BASELINE ESTACIONAL ===")

# Cargar credenciales BD
readRenviron('.Renviron')

# ==================== CONEXIÓN BD ====================
con <- NULL # Inicializar para el finally
tryCatch({
  con <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST"),
    port = Sys.getenv("DB_PORT"),
    dbname = Sys.getenv("DB_NAME"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
  
  log_success("✅ Conectado a PostgreSQL en {Sys.getenv('DB_HOST')}")
  
  # ==================== CREAR TABLA ====================
  log_info("\nCreando tabla dim_baseline_estacional...")
  
  dbExecute(con, "DROP TABLE IF EXISTS dim_baseline_estacional CASCADE;")
  
  # [FIX 6] Query formateada
  query_crear_tabla <- "
  CREATE TABLE dim_baseline_estacional (
    id_magnitud INT,
    mes INT,           -- 1-12
    dia_mes INT,       -- 1-31
    hora INT,          -- 0-23
    
    -- Promedios
    promedio_5y FLOAT,   -- Promedio últimos 5 años
    promedio_10y FLOAT,  -- Promedio últimos 10 años
    
    -- Percentiles
    p10 FLOAT,           -- Percentil 10
    p25 FLOAT,           -- Percentil 25 (Q1)
    p50 FLOAT,           -- Mediana
    p75 FLOAT,           -- Percentil 75 (Q3)
    p90 FLOAT,           -- Percentil 90
    
    -- Variabilidad
    desviacion_std FLOAT,
    coef_variacion FLOAT,
    
    -- Conteo
    n_observaciones_5y INT,
    n_observaciones_10y INT,
    
    -- Metadatos
    fecha_calculo TIMESTAMP DEFAULT NOW(),
    
    PRIMARY KEY (id_magnitud, mes, dia_mes, hora)
  )
  "
  dbExecute(con, query_crear_tabla)
  
  dbExecute(con, "CREATE INDEX idx_baseline_magnitud ON dim_baseline_estacional(id_magnitud)")
  dbExecute(con, "CREATE INDEX idx_baseline_fecha ON dim_baseline_estacional(mes, dia_mes, hora)")
  dbExecute(con, "COMMENT ON TABLE dim_baseline_estacional IS 'Baseline estacional: promedios históricos por (mes, día, hora) para predicción robusta'")
  log_success("✅ Tabla dim_baseline_estacional creada")
  
  # ==================== CONTAMINANTES ICA ====================
  
  # [FIX 2] Usar data.table()
  contaminantes_ica_dt <- data.table(
    id_magnitud = c(8, 9, 10, 14, 1),
    nombre = c("NO2", "PM10", "PM2.5", "O3", "SO2")
  )
  
  log_info("\nCalculando baselines para {nrow(contaminantes_ica_dt)} contaminantes ICA")
  log_info("Contaminantes: {paste(contaminantes_ica_dt$nombre, collapse=', ')}")
  
  # ==================== CALCULAR BASELINES ====================
  inicio_total <- Sys.time()
  
  contaminantes_ica_dt[, {
    
    log_info("\n--- Contaminante {.GRP}/{.N}: {nombre} (id={id_magnitud}) ---")
    inicio <- Sys.time()
    
    query_baseline <- glue("
    INSERT INTO dim_baseline_estacional (
      id_magnitud, mes, dia_mes, hora,
      promedio_5y, promedio_10y,
      p10, p25, p50, p75, p90,
      desviacion_std, coef_variacion,
      n_observaciones_5y, n_observaciones_10y
    )
    SELECT
      {id_magnitud} as id_magnitud,
      EXTRACT(MONTH FROM fecha_hora)::INT as mes,
      EXTRACT(DAY FROM fecha_hora)::INT as dia_mes,
      EXTRACT(HOUR FROM fecha_hora)::INT as hora,

      -- Promedios
      AVG(CASE WHEN fecha_hora >= NOW() - INTERVAL '5 years' THEN valor_medido END) as promedio_5y,
      AVG(valor_medido) as promedio_10y,

      -- Percentiles
      PERCENTILE_CONT(0.10) WITHIN GROUP (ORDER BY valor_medido) as p10,
      PERCENTILE_CONT(0.25) WITHIN GROUP (ORDER BY valor_medido) as p25,
      PERCENTILE_CONT(0.50) WITHIN GROUP (ORDER BY valor_medido) as p50,
      PERCENTILE_CONT(0.75) WITHIN GROUP (ORDER BY valor_medido) as p75,
      PERCENTILE_CONT(0.90) WITHIN GROUP (ORDER BY valor_medido) as p90,

      -- Variabilidad
      STDDEV(valor_medido) as desviacion_std,
      CASE
        WHEN AVG(valor_medido) > 0 THEN STDDEV(valor_medido) / AVG(valor_medido)
        ELSE NULL
      END as coef_variacion,

      -- Conteo
      COUNT(CASE WHEN fecha_hora >= NOW() - INTERVAL '5 years' THEN 1 END)::INT as n_observaciones_5y,
      COUNT(*)::INT as n_observaciones_10y

    FROM fact_mediciones
    WHERE id_magnitud = {id_magnitud}
      AND fecha_hora >= NOW() - INTERVAL '10 years'
      AND valor_medido > 0
      AND valor_medido < 1000  -- Filtrar outliers extremos
    GROUP BY mes, dia_mes, hora
    HAVING COUNT(*) >= 10 -- Al menos 10 observaciones

    -- [FIX 6] Formato de ON CONFLICT
    ON CONFLICT (id_magnitud, mes, dia_mes, hora) DO UPDATE
      SET promedio_5y = EXCLUDED.promedio_5y,
          promedio_10y = EXCLUDED.promedio_10y,
          p10 = EXCLUDED.p10,
          p25 = EXCLUDED.p25,
          p50 = EXCLUDED.p50,
          p75 = EXCLUDED.p75,
          p90 = EXCLUDED.p90,
          desviacion_std = EXCLUDED.desviacion_std,
          coef_variacion = EXCLUDED.coef_variacion,
          n_observaciones_5y = EXCLUDED.n_observaciones_5y,
          n_observaciones_10y = EXCLUDED.n_observaciones_10y,
          fecha_calculo = NOW();
    ")
    
    n_insertados <- dbExecute(con, query_baseline)
    tiempo <- difftime(Sys.time(), inicio, units = "secs")
    log_success("✅ {nombre}: {n_insertados} registros baseline insertados en {round(tiempo, 1)}s")
    
  }, by = .(id_magnitud, nombre)] # Iterar por cada grupo de contaminante
  
  
  tiempo_total <- difftime(Sys.time(), inicio_total, units = "mins")
  log_success("\n✅ TODOS LOS BASELINES CALCULADOS en {round(tiempo_total, 2)} minutos")
  
  # ==================== VALIDACIÓN Y RESUMEN ====================
  log_info("\n=== VALIDACIÓN Y RESUMEN ===")
  
  # (Las queries de validación son correctas y claras)
  resumen <- dbGetQuery(con, "
  SELECT
    dm.descripcion as contaminante,
    be.id_magnitud,
    COUNT(*) as n_combinaciones,
    ROUND(AVG(be.promedio_5y)::numeric, 2) as avg_promedio_5y,
    ROUND(AVG(be.n_observaciones_5y)::numeric, 0) as avg_obs_5y,
    MIN(be.n_observaciones_5y) as min_obs_5y,
    MAX(be.n_observaciones_5y) as max_obs_5y
  FROM dim_baseline_estacional be
  LEFT JOIN dim_magnitudes dm ON be.id_magnitud = dm.id_magnitud
  GROUP BY dm.descripcion, be.id_magnitud
  ORDER BY be.id_magnitud
  ")
  
  log_info("\nResumen baselines por contaminante:")
  print(resumen)
  
  cobertura <- dbGetQuery(con, "
  SELECT
    COUNT(DISTINCT mes) as meses_unicos,
    COUNT(DISTINCT dia_mes) as dias_unicos,
    COUNT(DISTINCT hora) as horas_unicas,
    COUNT(*) as total_registros
  FROM dim_baseline_estacional
  ")
  
  log_info("\nCobertura temporal:")
  log_info("  Meses únicos: {cobertura$meses_unicos} (esperado: 12)")
  log_info("  Días únicos: {cobertura$dias_unicos} (esperado: ~31)")
  log_info("  Horas únicas: {cobertura$horas_unicas} (esperado: 24)")
  log_info("  Total registros: {format(cobertura$total_registros, big.mark=',')}")
  
  
  # ==================== CREAR VISTA PARA USO FÁCIL ====================
  log_info("\n=== CREANDO VISTA FACILITADORA ===")
  
  dbExecute(con, "
  CREATE OR REPLACE VIEW v_baseline_estacional_actual AS
  SELECT
    be.*,
    dm.descripcion as nombre_contaminante,
    dm.unidad_medida,
    CASE
      WHEN be.promedio_5y < be.p25 THEN 'bajo'
      WHEN be.promedio_5y < be.p75 THEN 'medio'
      ELSE 'alto'
    END as categoria_baseline
  FROM dim_baseline_estacional be
  LEFT JOIN dim_magnitudes dm ON be.id_magnitud = dm.id_magnitud
  ")
  
  dbExecute(con, "COMMENT ON VIEW v_baseline_estacional_actual IS 'Vista con baselines + nombres de contaminantes para uso fácil'")
  log_success("✅ Vista v_baseline_estacional_actual creada")
  
  # ==================== EJEMPLO DE USO ====================
  log_info("\n=== EJEMPLO DE USO DEL BASELINE (Estilo data.table) ===")
  
  cat("
# Para usar el baseline en tus modelos (¡Estilo data.table!):

library(data.table)

# 1. Cargar baseline y establecer claves (¡rápido!)
baseline_dt <- setDT(dbGetQuery(con, \"
  SELECT id_magnitud, mes, dia_mes, hora, promedio_5y, p10, p90
  FROM dim_baseline_estacional
\"))
setkey(baseline_dt, id_magnitud, mes, dia_mes, hora)

# 2. Preparar datos_ml (suponiendo que es un data.table)
# Crear columnas temporales por referencia (:=)
datos_ml[, `:=`(
  mes = month(fecha_hora),
  dia_mes = mday(fecha_hora),
  hora = hour(fecha_hora)
)]

# 3. Realizar un join basado en claves (¡muy eficiente!)
# Actualiza datos_ml por referencia, añadiendo columnas de baseline_dt
datos_ml[baseline_dt, on = .(id_magnitud, mes, dia_mes, hora), `:=`(
  promedio_5y = i.promedio_5y,
  p10 = i.p10,
  p90 = i.p90
)]

# 4. Crear predictores derivados por referencia (:=)
datos_ml[!is.na(promedio_5y), `:=`(
  desviacion_baseline = valor_medio - promedio_5y,
  ratio_baseline = valor_medio / (promedio_5y + 0.1),
  fuera_rango_normal = fifelse(valor_medio < p10 | valor_medio > p90, 1, 0)
)]

# 5. Limpiar columnas temporales (opcional)
datos_ml[, `:=`(mes = NULL, dia_mes = NULL, hora = NULL)]

# Estas 3 nuevas variables son PREDICTORES potentes:
# - desviacion_baseline: ¿Cuánto se desvía del normal?
# - ratio_baseline: ¿Qué proporción del normal?
# - fuera_rango_normal: ¿Es un valor anómalo?
")
  
}, error = function(e) {
  log_error("Error fatal al crear baseline: {e$message}")
}, finally = {
  # ==================== DESCONECTAR ====================
  if (!is.null(con) && dbIsValid(con)) {
    dbDisconnect(con)
    log_info("\nConexión a BD cerrada.")
  }
})

log_success("\n🎉 PROCESO COMPLETADO EXITOSAMENTE")
log_info("Tabla: dim_baseline_estacional")
log_info("Vista: v_baseline_estacional_actual")
log_info("Tiempo total: {round(tiempo_total, 2)} minutos")
log_info("Próximo paso: Modificar los scripts de modelo para usar estos baselines")