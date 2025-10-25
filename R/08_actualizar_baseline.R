#!/usr/bin/env Rscript
# ACTUALIZAR BASELINE ESTACIONAL
# Script para actualizar dim_baseline_estacional cuando hay nuevos datos
# Diseñado para ejecutarse en GitHub Actions con mínimo uso de recursos

library(DBI)
library(RPostgres)
library(logger)
library(glue)

# Configurar logger simple para GitHub Actions
log_threshold(INFO)
log_info("=== ACTUALIZANDO BASELINE ESTACIONAL ===")

# Cargar credenciales desde variables de entorno
db_host <- Sys.getenv("DB_HOST")
db_port <- as.integer(Sys.getenv("DB_PORT"))
db_name <- Sys.getenv("DB_NAME")
db_user <- Sys.getenv("DB_USER")
db_password <- Sys.getenv("DB_PASSWORD")

# Validar credenciales
if (nchar(db_host) == 0 || nchar(db_name) == 0 || nchar(db_user) == 0) {
  log_error("Faltan variables de entorno de base de datos")
  quit(status = 1)
}

# Conectar a base de datos
log_info("Conectando a {db_host}...")
tryCatch({
  con <- dbConnect(
    RPostgres::Postgres(),
    host = db_host,
    port = db_port,
    dbname = db_name,
    user = db_user,
    password = db_password,
    sslmode = "require"
  )
  log_info("✅ Conectado exitosamente")
}, error = function(e) {
  log_error("Error de conexión: {e$message}")
  quit(status = 1)
})

# Verificar si hay datos nuevos desde la última actualización del baseline
log_info("Verificando si hay datos nuevos...")

ultima_actualizacion <- dbGetQuery(con, "
  SELECT MAX(fecha_calculo) as ultima_actualizacion
  FROM dim_baseline_estacional
")$ultima_actualizacion

if (is.na(ultima_actualizacion)) {
  log_warn("No hay baseline previo, se calculará desde cero")
  ultima_actualizacion <- as.POSIXct("2000-01-01", tz = "UTC")
}

datos_nuevos <- dbGetQuery(con, "
  SELECT COUNT(*) as n_nuevos
  FROM fact_mediciones
  WHERE fecha_hora > $1
", params = list(ultima_actualizacion))$n_nuevos

log_info("Datos nuevos desde última actualización: {datos_nuevos}")

# Si no hay suficientes datos nuevos, salir (ahorra recursos)
if (datos_nuevos < 100) {
  log_info("✅ Baseline actualizado (no hay suficientes datos nuevos)")
  dbDisconnect(con)
  quit(status = 0)
}

# Contaminantes ICA oficiales
contaminantes_ica <- data.frame(
  id_magnitud = c(1, 8, 9, 10, 14),
  nombre = c("SO2", "NO2", "PM2.5", "PM10", "O3")
)

log_info("Actualizando baselines para {nrow(contaminantes_ica)} contaminantes ICA...")

# Actualizar baselines para cada contaminante
total_actualizados <- 0

for (i in 1:nrow(contaminantes_ica)) {
  id_mag <- contaminantes_ica$id_magnitud[i]
  nombre <- contaminantes_ica$nombre[i]

  log_info("Procesando {nombre} (id={id_mag})...")

  tryCatch({
    # Query optimizada que usa UPSERT para actualizar o insertar
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
        AND valor_medido < 1000
      GROUP BY mes, dia_mes, hora
      HAVING COUNT(*) >= 10

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

    n_actualizados <- dbExecute(con, query_baseline)
    total_actualizados <- total_actualizados + n_actualizados
    log_info("✅ {nombre}: {n_actualizados} registros actualizados")

  }, error = function(e) {
    log_error("❌ Error en {nombre}: {e$message}")
  })
}

# Resumen final
log_info("=== RESUMEN ===")
log_info("Total registros actualizados: {total_actualizados}")

# Verificar estado final
resumen_final <- dbGetQuery(con, "
  SELECT
    COUNT(*) as total_registros,
    COUNT(DISTINCT id_magnitud) as contaminantes,
    MAX(fecha_calculo) as ultima_actualizacion
  FROM dim_baseline_estacional
")

log_info("Estado baseline:")
log_info("  - Total registros: {resumen_final$total_registros}")
log_info("  - Contaminantes: {resumen_final$contaminantes}")
log_info("  - Última actualización: {resumen_final$ultima_actualizacion}")

# Desconectar
dbDisconnect(con)
log_info("✅ ACTUALIZACIÓN BASELINE COMPLETADA")
