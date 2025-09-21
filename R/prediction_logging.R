# FUNCIONES PARA LOGGING DE PREDICCIONES
# Guardar predicciones para análisis retrospectivo del error del modelo

library(DBI)
library(dplyr)
library(lubridate)
library(logger)

# Configurar logging
log_threshold(INFO)

#' Guardar predicciones en log para análisis posterior
#'
#' @param predicciones data.frame con columnas: id_estacion, id_magnitud, fecha_hora, prediccion
#' @param version_modelo character versión del modelo utilizado
#' @param metodo_prediccion character método usado (ej: "CARET Random Forest")
#' @param fecha_prediccion POSIXct momento en que se hizo la predicción
#' @return logical TRUE si se guardó correctamente
guardar_predicciones_log <- function(predicciones, version_modelo = NULL, metodo_prediccion = "CARET Random Forest", fecha_prediccion = Sys.time()) {

  tryCatch({
    con <- conectar_bd()

    # Detectar versión del modelo automáticamente si no se proporciona
    if (is.null(version_modelo)) {
      version_modelo <- paste0("v", format(Sys.Date(), "%Y%m%d"))
    }

    # Preparar datos para insertar
    predicciones_log <- predicciones %>%
      mutate(
        fecha_prediccion = fecha_prediccion,
        fecha_objetivo = fecha_hora,
        valor_predicho = prediccion,
        version_modelo = version_modelo,
        metodo_prediccion = metodo_prediccion,

        # Calcular horizonte de horas
        horizonte_horas = as.numeric(difftime(fecha_objetivo, fecha_prediccion, units = "hours")),

        # Metadatos
        datos_entrada_completos = TRUE,  # Por ahora asumimos que sí
        confianza_modelo = NA_real_      # Implementar si el modelo da intervalos de confianza
      ) %>%
      select(
        fecha_prediccion, fecha_objetivo, id_estacion, id_magnitud,
        valor_predicho, confianza_modelo, version_modelo, metodo_prediccion,
        horizonte_horas, datos_entrada_completos
      )

    # Insertar en base de datos
    rows_inserted <- DBI::dbAppendTable(con, "log_predicciones", predicciones_log)

    log_info("✅ Guardadas {nrow(predicciones_log)} predicciones en log")
    log_info("📊 Versión modelo: {version_modelo}")
    log_info("🎯 Horizonte: {min(predicciones_log$horizonte_horas, na.rm = TRUE)}-{max(predicciones_log$horizonte_horas, na.rm = TRUE)} horas")

    DBI::dbDisconnect(con)
    return(TRUE)

  }, error = function(e) {
    log_error("❌ Error guardando predicciones: {e$message}")
    if (exists("con")) DBI::dbDisconnect(con)
    return(FALSE)
  })
}

#' Actualizar valores reales y calcular errores
#'
#' @param dias_atras integer días hacia atrás para buscar predicciones sin valor real
#' @return integer número de registros actualizados
actualizar_valores_reales <- function(dias_atras = 7) {

  tryCatch({
    con <- conectar_bd()

    # Llamar función SQL para actualizar valores reales
    resultado <- DBI::dbGetQuery(con, "SELECT actualizar_valores_reales() as registros_actualizados;")

    registros_actualizados <- resultado$registros_actualizados[1]

    log_info("🔄 Actualizados {registros_actualizados} registros con valores reales")

    DBI::dbDisconnect(con)
    return(registros_actualizados)

  }, error = function(e) {
    log_error("❌ Error actualizando valores reales: {e$message}")
    if (exists("con")) DBI::dbDisconnect(con)
    return(0)
  })
}

#' Obtener estadísticas de performance del modelo
#'
#' @param version_modelo character versión específica del modelo (NULL para todas)
#' @param meses_atras integer meses hacia atrás para el análisis
#' @return data.frame con métricas de performance
obtener_performance_modelo <- function(version_modelo = NULL, meses_atras = 6) {

  tryCatch({
    con <- conectar_bd()

    # Construir consulta
    query <- "
    SELECT
      de.nombre_estacion,
      dm.nombre_magnitud,
      dm.unidad,
      vpm.*
    FROM vista_performance_modelo vpm
    JOIN dim_estaciones de ON vpm.id_estacion = de.id_estacion
    JOIN dim_magnitudes dm ON vpm.id_magnitud = dm.id_magnitud
    WHERE vpm.mes_prediccion >= CURRENT_DATE - INTERVAL '{meses_atras} months'
    "

    if (!is.null(version_modelo)) {
      query <- paste0(query, " AND vpm.version_modelo = '{version_modelo}'")
    }

    query <- paste0(query, " ORDER BY vpm.mes_prediccion DESC, de.nombre_estacion, dm.nombre_magnitud")

    # Ejecutar consulta con interpolación
    query <- glue::glue(query)
    performance <- DBI::dbGetQuery(con, query)

    log_info("📊 Obtenidas estadísticas de {nrow(performance)} combinaciones estación-contaminante")

    DBI::dbDisconnect(con)
    return(performance)

  }, error = function(e) {
    log_error("❌ Error obteniendo performance: {e$message}")
    if (exists("con")) DBI::dbDisconnect(con)
    return(data.frame())
  })
}

#' Generar reporte de performance del modelo
#'
#' @param output_file character ruta del archivo de salida (opcional)
#' @return data.frame resumen de performance
generar_reporte_performance <- function(output_file = NULL) {

  tryCatch({
    # Obtener datos de performance
    performance <- obtener_performance_modelo()

    if (nrow(performance) == 0) {
      log_warn("⚠️ No hay datos de performance disponibles")
      return(data.frame())
    }

    # Resumen general
    resumen_general <- performance %>%
      group_by(version_modelo, nombre_magnitud) %>%
      summarise(
        estaciones = n_distinct(nombre_estacion),
        predicciones_totales = sum(total_predicciones, na.rm = TRUE),
        predicciones_con_real = sum(predicciones_con_real, na.rm = TRUE),
        mae_promedio = weighted.mean(mae, predicciones_con_real, na.rm = TRUE),
        rmse_promedio = weighted.mean(rmse, predicciones_con_real, na.rm = TRUE),
        cobertura = predicciones_con_real / predicciones_totales,
        .groups = "drop"
      ) %>%
      arrange(nombre_magnitud, version_modelo)

    # Performance por horizonte temporal
    performance_horizonte <- performance %>%
      group_by(nombre_magnitud, horizonte_horas) %>%
      summarise(
        mae_6h = weighted.mean(error_6h, predicciones_con_real, na.rm = TRUE),
        mae_24h = weighted.mean(error_24h, predicciones_con_real, na.rm = TRUE),
        mae_48h = weighted.mean(error_48h, predicciones_con_real, na.rm = TRUE),
        .groups = "drop"
      )

    # Crear resumen final
    resumen <- list(
      fecha_reporte = Sys.time(),
      resumen_general = resumen_general,
      performance_horizonte = performance_horizonte,
      datos_detallados = performance
    )

    # Guardar archivo si se especifica
    if (!is.null(output_file)) {
      saveRDS(resumen, output_file)
      log_info("💾 Reporte guardado en: {output_file}")
    }

    # Log de resultados principales
    log_info("📈 RESUMEN DE PERFORMANCE:")
    log_info("   Contaminantes analizados: {length(unique(performance$nombre_magnitud))}")
    log_info("   Estaciones: {length(unique(performance$nombre_estacion))}")
    log_info("   Versiones modelo: {length(unique(performance$version_modelo))}")

    mejores_mae <- resumen_general %>%
      group_by(nombre_magnitud) %>%
      slice_min(mae_promedio, n = 1)

    for (i in 1:nrow(mejores_mae)) {
      contaminante <- mejores_mae$nombre_magnitud[i]
      mae <- round(mejores_mae$mae_promedio[i], 2)
      version <- mejores_mae$version_modelo[i]
      log_info("   {contaminante}: MAE = {mae} (modelo {version})")
    }

    return(resumen)

  }, error = function(e) {
    log_error("❌ Error generando reporte: {e$message}")
    return(list())
  })
}

#' Obtener predicciones recientes para monitoreo
#'
#' @param dias integer días hacia atrás para buscar predicciones
#' @return data.frame con predicciones recientes y su estado
obtener_monitoreo_predicciones <- function(dias = 7) {

  tryCatch({
    con <- conectar_bd()

    query <- glue::glue("
    SELECT * FROM vista_monitoreo_predicciones
    WHERE fecha_prediccion >= CURRENT_DATE - INTERVAL '{dias} days'
    ORDER BY fecha_prediccion DESC
    LIMIT 1000
    ")

    monitoreo <- DBI::dbGetQuery(con, query)

    DBI::dbDisconnect(con)
    return(monitoreo)

  }, error = function(e) {
    log_error("❌ Error obteniendo monitoreo: {e$message}")
    if (exists("con")) DBI::dbDisconnect(con)
    return(data.frame())
  })
}

# Función de conveniencia para uso en scripts principales
log_predictions_wrapper <- function(predicciones_file = "output/predicciones_40h_latest.rds") {

  if (!file.exists(predicciones_file)) {
    log_warn("⚠️ No se encuentra archivo de predicciones: {predicciones_file}")
    return(FALSE)
  }

  # Cargar predicciones
  predicciones <- readRDS(predicciones_file)

  # Guardar en log
  success <- guardar_predicciones_log(
    predicciones = predicciones,
    version_modelo = paste0("v", format(Sys.Date(), "%Y%m%d")),
    metodo_prediccion = "CARET Random Forest Ensemble"
  )

  # Actualizar valores reales si es posible
  if (success) {
    actualizar_valores_reales()
  }

  return(success)
}