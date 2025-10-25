# CREAR TABLA BASELINE ESTACIONAL
# Objetivo: Calcular promedios históricos por (mes, día, hora) para cada contaminante
# Período: Últimos 5 y 10 años de datos
# Uso: Baseline robusto para predicciones (evita valores anómalos)

library(DBI)
library(RPostgres)
library(dplyr)
library(logger)
library(glue)

# ==================== CONFIGURACIÓN ====================
log_threshold(INFO)
log_appender(appender_tee("logs/baseline_estacional.log"))
log_info("=== CREANDO TABLA BASELINE ESTACIONAL ===")

# Cargar credenciales BD
readRenviron('.Renviron')

# ==================== CONEXIÓN BD ====================
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

# Eliminar tabla si existe
dbExecute(con, "DROP TABLE IF EXISTS dim_baseline_estacional CASCADE;")

# Crear tabla
query_crear_tabla <- "
  CREATE TABLE dim_baseline_estacional (
    id_magnitud INT,
    mes INT,              -- 1-12
    dia_mes INT,          -- 1-31
    hora INT,             -- 0-23

    -- Promedios
    promedio_5y FLOAT,    -- Promedio últimos 5 años
    promedio_10y FLOAT,   -- Promedio últimos 10 años

    -- Percentiles (para detectar valores anómalos)
    p10 FLOAT,            -- Percentil 10
    p25 FLOAT,            -- Percentil 25 (Q1)
    p50 FLOAT,            -- Mediana
    p75 FLOAT,            -- Percentil 75 (Q3)
    p90 FLOAT,            -- Percentil 90

    -- Variabilidad
    desviacion_std FLOAT, -- Desviación estándar
    coef_variacion FLOAT, -- Coeficiente de variación (SD/media)

    -- Cantidad de datos
    n_observaciones_5y INT,
    n_observaciones_10y INT,

    -- Metadatos
    fecha_calculo TIMESTAMP DEFAULT NOW(),

    PRIMARY KEY (id_magnitud, mes, dia_mes, hora)
  )
"

dbExecute(con, query_crear_tabla)

# Crear índices (separado)
dbExecute(con, "CREATE INDEX idx_baseline_magnitud ON dim_baseline_estacional(id_magnitud)")
dbExecute(con, "CREATE INDEX idx_baseline_fecha ON dim_baseline_estacional(mes, dia_mes, hora)")

# Comentario (separado)
dbExecute(con, "COMMENT ON TABLE dim_baseline_estacional IS 'Baseline estacional: promedios históricos por (mes, día, hora) para predicción robusta'")
log_success("✅ Tabla dim_baseline_estacional creada")

# ==================== CONTAMINANTES ICA ====================
# Solo calcular para los 5 contaminantes del Índice de Calidad del Aire

contaminantes_ica <- data.frame(
  id_magnitud = c(8, 9, 10, 14, 1),
  nombre = c("NO2", "PM10", "PM2.5", "O3", "SO2")
)

log_info("\nCalculando baselines para {nrow(contaminantes_ica)} contaminantes ICA")
log_info("Contaminantes: {paste(contaminantes_ica$nombre, collapse=', ')}")

# ==================== CALCULAR BASELINES ====================
inicio_total <- Sys.time()

for(i in 1:nrow(contaminantes_ica)) {

  id_mag <- contaminantes_ica$id_magnitud[i]
  nombre <- contaminantes_ica$nombre[i]

  log_info("\n--- Contaminante {i}/{nrow(contaminantes_ica)}: {nombre} (id={id_mag}) ---")

  inicio <- Sys.time()

  # Query para calcular todas las estadísticas
  query_baseline <- glue("
    INSERT INTO dim_baseline_estacional (
      id_magnitud, mes, dia_mes, hora,
      promedio_5y, promedio_10y,
      p10, p25, p50, p75, p90,
      desviacion_std, coef_variacion,
      n_observaciones_5y, n_observaciones_10y
    )
    SELECT
      {id_mag} as id_magnitud,
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

      -- Cantidad de observaciones
      COUNT(CASE WHEN fecha_hora >= NOW() - INTERVAL '5 years' THEN 1 END)::INT as n_observaciones_5y,
      COUNT(*)::INT as n_observaciones_10y

    FROM fact_mediciones
    WHERE id_magnitud = {id_mag}
      AND fecha_hora >= NOW() - INTERVAL '10 years'
      AND valor_medido > 0
      AND valor_medido < 1000  -- Filtrar outliers extremos
    GROUP BY mes, dia_mes, hora
    HAVING COUNT(*) >= 10  -- Al menos 10 observaciones para calcular baseline

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

  # Ejecutar query
  n_insertados <- dbExecute(con, query_baseline)

  tiempo <- difftime(Sys.time(), inicio, units = "secs")

  log_success("✅ {nombre}: {n_insertados} registros baseline insertados en {round(tiempo, 1)}s")
}

tiempo_total <- difftime(Sys.time(), inicio_total, units = "mins")
log_success("\n✅ TODOS LOS BASELINES CALCULADOS en {round(tiempo_total, 2)} minutos")

# ==================== VALIDACIÓN Y RESUMEN ====================
log_info("\n=== VALIDACIÓN Y RESUMEN ===")

# Resumen por contaminante
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

# Verificar cobertura temporal
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

# Verificar registros con pocos datos
pocos_datos <- dbGetQuery(con, "
  SELECT COUNT(*) as n_registros
  FROM dim_baseline_estacional
  WHERE n_observaciones_5y < 50
")

if(pocos_datos$n_registros > 0) {
  log_warn("⚠️ {pocos_datos$n_registros} registros con < 50 observaciones en 5 años")
  log_warn("   Estos registros pueden tener baselines menos confiables")
} else {
  log_success("✅ Todos los registros tienen ≥ 50 observaciones")
}

# ==================== EJEMPLOS DE BASELINES ====================
log_info("\n=== EJEMPLOS DE BASELINES CALCULADOS ===")

# Ejemplo 1: NO2 en hora pico (Lunes 08:00, día típico octubre)
ejemplo1 <- dbGetQuery(con, "
  SELECT
    dm.descripcion as contaminante,
    mes,
    dia_mes,
    hora,
    ROUND(promedio_5y::numeric, 2) as promedio_5y,
    ROUND(p10::numeric, 2) as p10,
    ROUND(p50::numeric, 2) as mediana,
    ROUND(p90::numeric, 2) as p90,
    n_observaciones_5y
  FROM dim_baseline_estacional be
  LEFT JOIN dim_magnitudes dm ON be.id_magnitud = dm.id_magnitud
  WHERE be.id_magnitud = 8  -- NO2
    AND mes = 10            -- Octubre
    AND dia_mes = 11        -- Día 11
    AND hora = 8            -- 08:00 (hora pico)
")

if(nrow(ejemplo1) > 0) {
  log_info("\nEjemplo 1: NO2 - 11 octubre, 08:00 (hora pico tráfico)")
  print(ejemplo1)
  log_info("  Interpretación: Promedio histórico {ejemplo1$promedio_5y} µg/m³")
  log_info("  Rango normal: {ejemplo1$p10} - {ejemplo1$p90} µg/m³ (P10-P90)")
}

# Ejemplo 2: O3 en mediodía verano (cuando suele ser máximo)
ejemplo2 <- dbGetQuery(con, "
  SELECT
    dm.descripcion as contaminante,
    mes,
    dia_mes,
    hora,
    ROUND(promedio_5y::numeric, 2) as promedio_5y,
    ROUND(p10::numeric, 2) as p10,
    ROUND(p90::numeric, 2) as p90,
    n_observaciones_5y
  FROM dim_baseline_estacional be
  LEFT JOIN dim_magnitudes dm ON be.id_magnitud = dm.id_magnitud
  WHERE be.id_magnitud = 14  -- O3
    AND mes = 7              -- Julio
    AND dia_mes = 15         -- Día 15
    AND hora = 14            -- 14:00 (máxima radiación solar)
  LIMIT 1
")

if(nrow(ejemplo2) > 0) {
  log_info("\nEjemplo 2: Ozono - 15 julio, 14:00 (máxima radiación solar)")
  print(ejemplo2)
  log_info("  Interpretación: O3 suele estar alto en verano mediodía")
}

# ==================== CREAR VISTA PARA USO FÁCIL ====================
log_info("\n=== CREANDO VISTA FACILITADORA ===")

dbExecute(con, "
  CREATE OR REPLACE VIEW v_baseline_estacional_actual AS
  SELECT
    be.*,
    dm.descripcion as nombre_contaminante,
    dm.unidad_medida,
    -- Categorización del baseline
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
log_info("\n=== EJEMPLO DE USO DEL BASELINE ===")

cat("
# Para usar el baseline en tus modelos:

# En R (durante entrenamiento o predicción):
baseline <- dbGetQuery(con, \"
  SELECT
    id_magnitud,
    mes,
    dia_mes as dia,
    hora,
    promedio_5y,
    p10,
    p90
  FROM dim_baseline_estacional
\")

# Unir con tus datos de entrenamiento:
datos_ml <- datos_ml %>%
  mutate(
    mes = month(fecha_hora),
    dia = day(fecha_hora),
    hora = hour(fecha_hora)
  ) %>%
  left_join(
    baseline,
    by = c(\"id_magnitud\", \"mes\", \"dia\", \"hora\")
  ) %>%
  mutate(
    # Crear variables derivadas
    desviacion_baseline = valor_medio - promedio_5y,
    ratio_baseline = valor_medio / (promedio_5y + 0.1),
    fuera_rango_normal = ifelse(valor_medio < p10 | valor_medio > p90, 1, 0)
  )

# Estas 3 nuevas variables son PREDICTORES potentes:
# - desviacion_baseline: ¿Cuánto se desvía del normal? (+10 = 10 µg/m³ sobre el promedio)
# - ratio_baseline: ¿Qué proporción del normal? (1.5 = 50% más alto que lo normal)
# - fuera_rango_normal: ¿Es un valor anómalo? (1 = sí, 0 = no)
")

# ==================== DESCONECTAR ====================
dbDisconnect(con)

log_success("\n🎉 PROCESO COMPLETADO EXITOSAMENTE")
log_info("Tabla: dim_baseline_estacional")
log_info("Vista: v_baseline_estacional_actual")
log_info("Total registros: {format(cobertura$total_registros, big.mark=',')}")
log_info("Tiempo total: {round(tiempo_total, 2)} minutos")
log_info("\nPróximo paso: Modificar R/02_modelo_ranger_ica.R para usar estos baselines")
