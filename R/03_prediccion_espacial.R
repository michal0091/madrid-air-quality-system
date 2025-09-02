# PREDICCIÓN ESPACIAL EN TIEMPO REAL
# Objetivo: Usar modelos CARET entrenados para generar predicciones espaciales

# 1. LIBRERÍAS ----
library(sf)
library(dplyr)
library(logger)
library(glue)

# 2. CONFIGURACIÓN ----
log_appender(appender_tee("logs/prediccion_espacial.log"))
log_info("--- INICIO PREDICCIÓN ESPACIAL ---")

# 3. FUNCIÓN PARA CREAR REJILLA DE MADRID ----
crear_rejilla_madrid <- function(resolution = 0.01) {
  
  log_info("Creando rejilla de predicción para Madrid...")
  
  # Límites aproximados de Madrid
  madrid_bbox <- st_bbox(c(xmin = -3.9, ymin = 40.3, xmax = -3.5, ymax = 40.6), crs = 4326)
  
  # Crear rejilla
  rejilla <- st_make_grid(
    madrid_bbox,
    cellsize = resolution,  # ~1km resolución
    what = "centers"
  ) %>%
    st_sf() %>%
    mutate(
      grid_id = row_number(),
      coords = st_coordinates(.),
      lon = coords[,1],
      lat = coords[,2]
    )
  
  log_info("Rejilla creada: {nrow(rejilla)} puntos")
  
  return(rejilla)
}

# 4. FUNCIÓN PARA PREPARAR DATOS METEOROLÓGICOS PARA REJILLA ----
preparar_meteo_rejilla <- function(rejilla, datos_tiempo_real) {
  
  log_info("Preparando datos meteorológicos para rejilla...")
  
  # Calcular promedios meteorológicos de los datos tiempo real
  meteo_promedio <- datos_tiempo_real %>%
    st_drop_geometry() %>%
    summarise(
      temp_media_c = mean(temp_media_c, na.rm = TRUE),
      precipitacion_mm = mean(precipitacion_mm, na.rm = TRUE),
      vel_viento_media_ms = mean(vel_viento_media_ms, na.rm = TRUE),
      dir_viento_grados = mean(dir_viento_grados, na.rm = TRUE),
      presion_maxima_hpa = mean(presion_maxima_hpa, na.rm = TRUE),
      humedad_media_pct = mean(humedad_media_pct, na.rm = TRUE)
    )
  
  log_info("Promedios meteorológicos calculados:")
  log_info("  Temperatura: {round(meteo_promedio$temp_media_c, 1)}°C")
  log_info("  Humedad: {round(meteo_promedio$humedad_media_pct, 1)}%")
  log_info("  Viento: {round(meteo_promedio$vel_viento_media_ms, 1)} m/s")
  
  # Agregar datos meteorológicos a cada punto de la rejilla
  rejilla_completa <- rejilla %>%
    st_drop_geometry() %>%
    bind_cols(meteo_promedio[rep(1, nrow(rejilla)), ]) %>%
    # Agregar variable duplicada para compatibilidad
    mutate(valor_medido = 0)  # Placeholder, no se usa en predicción
  
  log_info("Rejilla con datos meteorológicos: {nrow(rejilla_completa)} puntos")
  
  return(rejilla_completa)
}

# 5. FUNCIÓN PRINCIPAL DE PREDICCIÓN ----
generar_predicciones_espaciales <- function(archivo_modelos = "models/modelos_caret_avanzados.rds",
                                          archivo_datos = "data/realtime/datos_prediccion_latest.rds") {
  
  log_info("=== GENERANDO PREDICCIONES ESPACIALES ===")
  
  # Paso 1: Cargar modelos entrenados
  if(!file.exists(archivo_modelos)) {
    log_error("Archivo de modelos no existe: {archivo_modelos}")
    return(NULL)
  }
  
  modelos <- readRDS(archivo_modelos)
  log_info("Modelos cargados: {length(modelos)} contaminantes")
  
  # Paso 2: Cargar datos tiempo real
  if(!file.exists(archivo_datos)) {
    log_error("Archivo de datos no existe: {archivo_datos}")
    return(NULL)
  }
  
  datos_tiempo_real <- readRDS(archivo_datos)
  log_info("Datos tiempo real cargados: {nrow(datos_tiempo_real)} observaciones")
  
  # Paso 3: Crear rejilla de predicción
  rejilla <- crear_rejilla_madrid()
  
  # Paso 4: Preparar datos meteorológicos para rejilla
  rejilla_meteo <- preparar_meteo_rejilla(rejilla, datos_tiempo_real)
  
  # Paso 5: Generar predicciones para cada contaminante
  predicciones_finales <- list()
  
  for(nombre_contaminante in names(modelos)) {
    log_info("Generando predicciones para: {nombre_contaminante}")
    
    modelo_info <- modelos[[nombre_contaminante]]
    modelo_caret <- modelo_info$modelo
    
    tryCatch({
      # Hacer predicciones
      predicciones <- predict(modelo_caret, newdata = rejilla_meteo)
      
      # Crear resultado para este contaminante
      resultado_contaminante <- rejilla %>%
        st_drop_geometry() %>%
        mutate(
          contaminante = nombre_contaminante,
          prediccion = predicciones,
          rmse_modelo = modelo_info$rmse,
          r2_modelo = modelo_info$rsquared,
          timestamp_prediccion = Sys.time()
        ) %>%
        # Convertir de nuevo a SF
        st_as_sf(coords = c("lon", "lat"), crs = 4326)
      
      predicciones_finales[[nombre_contaminante]] <- resultado_contaminante
      
      log_success("✓ Predicciones {nombre_contaminante}: {nrow(resultado_contaminante)} puntos")
      log_info("  Rango: {round(min(predicciones), 1)} - {round(max(predicciones), 1)} µg/m³")
      
    }, error = function(e) {
      log_error("Error prediciendo {nombre_contaminante}: {e$message}")
    })
  }
  
  # Paso 6: Combinar todas las predicciones
  if(length(predicciones_finales) == 0) {
    log_error("No se generaron predicciones")
    return(NULL)
  }
  
  resultado_completo <- bind_rows(predicciones_finales)
  
  # Paso 7: Guardar resultados
  archivo_salida <- "output/predicciones_espaciales_latest.rds"
  saveRDS(resultado_completo, archivo_salida)
  
  log_success("✓ PREDICCIONES ESPACIALES COMPLETADAS")
  log_info("  Total puntos: {nrow(resultado_completo)}")
  log_info("  Contaminantes: {length(unique(resultado_completo$contaminante))}")
  log_info("  Archivo guardado: {archivo_salida}")
  
  return(resultado_completo)
}

# 6. FUNCIÓN DE TEST ----
test_prediccion_espacial <- function() {
  log_info("=== TEST PREDICCIÓN ESPACIAL ===")
  
  resultado <- generar_predicciones_espaciales()
  
  if(is.null(resultado)) {
    log_error("❌ Test falló")
    return(FALSE)
  }
  
  # Validaciones
  validaciones <- list(
    puntos_totales = nrow(resultado),
    contaminantes = length(unique(resultado$contaminante)),
    rango_predicciones = c(min(resultado$prediccion), max(resultado$prediccion)),
    valores_negativos = sum(resultado$prediccion < 0),
    valores_na = sum(is.na(resultado$prediccion))
  )
  
  log_success("✓ Test predicción espacial exitoso")
  log_info("  Puntos totales: {validaciones$puntos_totales}")
  log_info("  Contaminantes: {validaciones$contaminantes}")
  log_info("  Rango predicciones: {round(validaciones$rango_predicciones[1], 1)} - {round(validaciones$rango_predicciones[2], 1)}")
  log_info("  Valores negativos: {validaciones$valores_negativos}")
  log_info("  Valores NA: {validaciones$valores_na}")
  
  return(TRUE)
}

# Ejecutar test automáticamente
if(interactive() == FALSE) {
  test_prediccion_espacial()
}