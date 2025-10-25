#!/usr/bin/env Rscript
# VALIDAR PARÁMETROS ÓPTIMOS PARA MODELOS RANGER
# Objetivo: Encontrar balance entre rendimiento y tamaño del modelo
# Probar con NO2 (modelo más grande) y extrapolar a los demás

library(caret)
library(ranger)
library(dplyr)
library(lubridate)
library(logger)
library(sf)

log_threshold(INFO)
log_info("=== VALIDACIÓN PARÁMETROS RANGER ===")

# Cargar datos de entrenamiento para NO2
log_info("Cargando datos de entrenamiento para NO2...")

if (!file.exists("models/datos_ml.rds")) {
  log_error("Archivo models/datos_ml.rds no encontrado")
  log_info("Ejecuta primero: source('R/01c_create_predictors.R')")
  quit(status = 1)
}

datos_completos <- readRDS("models/datos_ml.rds")

# Filtrar solo NO2 (usar data.table syntax para evitar conflictos con sf)
log_info("Filtrando datos de NO2...")
datos_no2 <- as.data.frame(datos_completos[datos_completos$nombre_magnitud == "Dióxido de Nitrógeno", ])

log_info("Datos NO2 cargados: {format(nrow(datos_no2), big.mark=',')} registros")

# Preparar datos para ranger
# Eliminar columnas no numéricas y geometry
columnas_eliminar <- c("nombre_estacion", "nombre_magnitud", "fecha_hora", "fecha",
                       "tipo_estacion", "unidad", "id_estacion", "id_magnitud",
                       "geometry", "coords", "valor_medido")

datos_no2 <- datos_no2[, !names(datos_no2) %in% columnas_eliminar, drop = FALSE]
datos_no2 <- datos_no2[!is.na(datos_no2$valor_medio), ]

log_info("Datos preparados: {ncol(datos_no2)} variables, {nrow(datos_no2)} observaciones")

# Configuraciones a probar
configuraciones <- data.frame(
  ntree = c(100, 200, 300, 400, 500),
  max.depth = c(20, 25, 25, 30, 30),
  min.node.size = c(10, 8, 5, 5, 5)
)

log_info("Probando {nrow(configuraciones)} configuraciones diferentes...")

resultados <- list()

for (i in 1:nrow(configuraciones)) {
  config <- configuraciones[i,]

  log_info("\n=== Configuración {i}/{nrow(configuraciones)} ===")
  log_info("ntree: {config$ntree}, max.depth: {config$max.depth}, min.node.size: {config$min.node.size}")

  inicio <- Sys.time()

  # Entrenar modelo con esta configuración
  tryCatch({
    modelo <- ranger(
      valor_medio ~ .,
      data = datos_no2,
      num.trees = config$ntree,
      max.depth = config$max.depth,
      min.node.size = config$min.node.size,
      importance = "impurity",
      num.threads = 4,
      verbose = FALSE,
      seed = 42
    )

    tiempo_entrenamiento <- as.numeric(difftime(Sys.time(), inicio, units = "secs"))

    # Calcular métricas
    predicciones <- predict(modelo, datos_no2)$predictions
    rmse <- sqrt(mean((datos_no2$valor_medio - predicciones)^2))
    mae <- mean(abs(datos_no2$valor_medio - predicciones))
    r2 <- 1 - sum((datos_no2$valor_medio - predicciones)^2) /
              sum((datos_no2$valor_medio - mean(datos_no2$valor_medio))^2)

    # Guardar modelo temporalmente y medir tamaño
    temp_file <- tempfile(fileext = ".rds")
    saveRDS(modelo, temp_file)
    tamano_mb <- file.info(temp_file)$size / 1024^2
    unlink(temp_file)

    # Estimar memoria en RAM (aproximadamente 4-5x el tamaño del archivo)
    memoria_estimada_gb <- (tamano_mb / 1024) * 4.5

    log_info("Resultados:")
    log_info("  RMSE: {round(rmse, 3)}")
    log_info("  MAE: {round(mae, 3)}")
    log_info("  R²: {round(r2, 4)}")
    log_info("  Tamaño archivo: {round(tamano_mb, 1)} MB")
    log_info("  Memoria estimada: {round(memoria_estimada_gb, 2)} GB")
    log_info("  Tiempo: {round(tiempo_entrenamiento, 1)} segundos")

    # Calcular eficiencia (R² por GB de memoria)
    eficiencia <- r2 / memoria_estimada_gb

    resultados[[i]] <- data.frame(
      config_id = i,
      ntree = config$ntree,
      max_depth = config$max.depth,
      min_node_size = config$min.node.size,
      rmse = rmse,
      mae = mae,
      r2 = r2,
      tamano_mb = tamano_mb,
      memoria_estimada_gb = memoria_estimada_gb,
      tiempo_segundos = tiempo_entrenamiento,
      eficiencia = eficiencia,
      cabe_en_github = memoria_estimada_gb < 6.5  # Dejar margen de 0.5GB
    )

    # Limpiar
    rm(modelo, predicciones)
    gc(verbose = FALSE)

  }, error = function(e) {
    log_error("Error en configuración {i}: {e$message}")
  })
}

# Combinar resultados
df_resultados <- bind_rows(resultados)

log_info("\n=== RESUMEN DE RESULTADOS ===")
print(df_resultados, row.names = FALSE)

# Identificar configuración óptima
# Criterio: Cabe en GitHub Actions Y máxima eficiencia (R² / GB)
configs_viables <- df_resultados %>%
  filter(cabe_en_github == TRUE) %>%
  arrange(desc(eficiencia))

if (nrow(configs_viables) > 0) {
  optimo <- configs_viables[1,]

  log_info("\n=== CONFIGURACIÓN ÓPTIMA RECOMENDADA ===")
  log_info("ID: {optimo$config_id}")
  log_info("ntree: {optimo$ntree}")
  log_info("max.depth: {optimo$max_depth}")
  log_info("min.node.size: {optimo$min_node_size}")
  log_info("")
  log_info("Rendimiento:")
  log_info("  RMSE: {round(optimo$rmse, 3)} µg/m³")
  log_info("  R²: {round(optimo$r2, 4)}")
  log_info("")
  log_info("Recursos:")
  log_info("  Tamaño archivo: {round(optimo$tamano_mb, 1)} MB")
  log_info("  Memoria estimada: {round(optimo$memoria_estimada_gb, 2)} GB")
  log_info("  ✅ Cabe en GitHub Actions (< 6.5 GB)")
  log_info("")
  log_info("Eficiencia: {round(optimo$eficiencia, 4)} (R²/GB)")

  # Comparar con configuración máxima (500 árboles)
  config_maxima <- df_resultados %>% filter(ntree == 500)

  if (nrow(config_maxima) > 0) {
    perdida_r2 <- (config_maxima$r2 - optimo$r2) / config_maxima$r2 * 100
    reduccion_memoria <- (config_maxima$memoria_estimada_gb - optimo$memoria_estimada_gb) /
                          config_maxima$memoria_estimada_gb * 100

    log_info("\n=== COMPARACIÓN CON MODELO MÁXIMO (500 árboles) ===")
    log_info("Pérdida de R²: {round(perdida_r2, 2)}%")
    log_info("Reducción de memoria: {round(reduccion_memoria, 1)}%")

    if (perdida_r2 < 2 && reduccion_memoria > 20) {
      log_success("✅ EXCELENTE: Reducción significativa de memoria con pérdida mínima de precisión")
    } else if (perdida_r2 < 5) {
      log_info("✓ ACEPTABLE: Balance razonable entre precisión y recursos")
    } else {
      log_warn("⚠️ ADVERTENCIA: Pérdida de precisión considerable")
    }
  }

  # Guardar resultados
  saveRDS(list(
    resultados = df_resultados,
    optimo = optimo,
    fecha = Sys.time()
  ), "models/validacion_parametros_ranger.rds")

  log_success("✅ Resultados guardados en models/validacion_parametros_ranger.rds")

} else {
  log_error("❌ Ninguna configuración cabe en GitHub Actions límite de memoria")
  log_info("Considera usar self-hosted runner o reducir aún más los parámetros")
}

log_info("\n=== VALIDACIÓN COMPLETADA ===")
