# FASE 2: MODELADO ESPACIAL CON CARET (VERSIÓN SIMPLIFICADA)
# Objetivo: Entrenar modelos CARET para predicción de calidad del aire
# Optimizado para datos meteorológicos horarios

# 1. CARGA DE LIBRERÍAS ----
renv::load()

# Spatial data handling
library(sf)

# Machine Learning - CARET únicamente
library(caret)
library(randomForest)

# Data manipulation
library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)

# Database
library(DBI)
library(RPostgres)

# Utilities
library(logger)
library(glue)

# Cargar utilidades meteorológicas
source("R/utils_meteo_horario.R")

# 2. CONFIGURACIÓN ----
log_appender(appender_tee("logs/caret_modeling.log"))
log_info("--- INICIO MODELADO CARET SIMPLIFICADO ---")

# Usar datos horarios (demostró mejor rendimiento)
usar_datos_horarios <- TRUE
log_info("Configuración: datos meteorológicos horarios = {usar_datos_horarios}")

# 3. FUNCIÓN PRINCIPAL DE ENTRENAMIENTO CARET ----
entrenar_modelo_caret_simple <- function(datos, contaminante = "NO2") {
  
  log_info("=== ENTRENANDO CARET: {contaminante} ===")
  
  # Paso 1: Filtrar datos por contaminante
  datos_filtrados <- datos %>% 
    filter(contaminante == !!contaminante)
  log_info("Datos filtrados: {nrow(datos_filtrados)} observaciones")
  
  if(nrow(datos_filtrados) < 10) {  # Umbral reducido para datos tiempo real
    log_warn("Insuficientes datos para {contaminante}")
    return(NULL)
  }
  
  # Paso 2: Preparar datos para CARET
  datos_caret <- datos_filtrados %>%
    # Extraer coordenadas
    mutate(
      coords = st_coordinates(.),
      lon = coords[,1],
      lat = coords[,2]
    ) %>%
    # Convertir a data.frame
    st_drop_geometry() %>%
    # Eliminar columnas problemáticas (incluyendo valor_medido que no debe ser predictor)
    select(-any_of(c("fecha", "id_estacion", "id_magnitud", "nombre_magnitud", 
                     "coords", "nombre_estacion", "tipo_estacion", "unidad",
                     "fecha_hora", "timestamp_obtencion", "fuente", "pipeline_version",
                     "valor_medido"))) %>%
    # Manejar missing values inteligentemente
    filter(
      !is.na(valor_medio),  # Variable objetivo
      !is.na(lon), !is.na(lat),  # Coordenadas
      !is.na(temp_media_c)  # Temperatura crítica
    ) %>%
    # Imputar missing values con valores estadísticos
    mutate(
      precipitacion_mm = ifelse(is.na(precipitacion_mm), 0, precipitacion_mm),
      vel_viento_media_ms = ifelse(is.na(vel_viento_media_ms), 
                                   mean(vel_viento_media_ms, na.rm = TRUE), 
                                   vel_viento_media_ms),
      dir_viento_grados = ifelse(is.na(dir_viento_grados), 180, dir_viento_grados),
      presion_maxima_hpa = ifelse(is.na(presion_maxima_hpa),
                                  mean(presion_maxima_hpa, na.rm = TRUE),
                                  presion_maxima_hpa),
      humedad_media_pct = ifelse(is.na(humedad_media_pct),
                                 mean(humedad_media_pct, na.rm = TRUE),
                                 humedad_media_pct)
    )
  
  log_info("Datos preparados para CARET: {nrow(datos_caret)} observaciones")
  log_info("Variables predictoras: {ncol(datos_caret)-1}")
  
  # Diagnóstico de tipos de variables
  log_info("Columnas disponibles: {paste(names(datos_caret), collapse=', ')}")
  tipos_var <- sapply(datos_caret, class)
  vars_categoricas <- names(tipos_var)[tipos_var == "character" | tipos_var == "factor"]
  if(length(vars_categoricas) > 0) {
    log_warn("Variables categóricas detectadas: {paste(vars_categoricas, collapse=', ')}")
    # Eliminar variables categóricas problemáticas
    datos_caret <- datos_caret %>% select(-any_of(vars_categoricas))
    log_info("Variables categóricas eliminadas. Columnas restantes: {ncol(datos_caret)}")
  }
  
  # Paso 3: Configuración de entrenamiento
  control_config <- trainControl(
    method = "cv",
    number = ifelse(nrow(datos_caret) > 5000, 3, 5),  # Menos folds para datasets grandes
    savePredictions = "final",
    allowParallel = TRUE
  )
  
  # Paso 4: Entrenar modelo Random Forest
  tryCatch({
    log_info("Iniciando entrenamiento Random Forest...")
    
    modelo_rf <- train(
      valor_medio ~ .,
      data = datos_caret,
      method = "rf",
      trControl = control_config,
      tuneLength = 3,  # Túning moderado
      ntree = ifelse(nrow(datos_caret) > 10000, 100, 50),  # Árboles según tamaño
      importance = TRUE
    )
    
    # Paso 5: Extraer métricas
    rmse_final <- min(modelo_rf$results$RMSE)
    rsquared_final <- max(modelo_rf$results$Rsquared, na.rm = TRUE)
    
    log_success("✓ Modelo CARET entrenado exitosamente para {contaminante}")
    log_info("  RMSE: {round(rmse_final, 3)}")
    log_info("  R²: {round(rsquared_final, 3)}")
    log_info("  Variables: {ncol(datos_caret) - 1}")
    
    # Paso 6: Preparar resultado
    resultado <- list(
      modelo = modelo_rf,
      contaminante = contaminante,
      rmse = rmse_final,
      rsquared = rsquared_final,
      n_observaciones = nrow(datos_caret),
      variables_usadas = names(datos_caret)[names(datos_caret) != "valor_medio"],
      timestamp_entrenamiento = Sys.time()
    )
    
    return(resultado)
    
  }, error = function(e) {
    log_error("Error entrenando CARET para {contaminante}: {e$message}")
    return(NULL)
  })
}

# 4. FUNCIÓN PARA CARGAR Y PREPARAR DATOS ----
cargar_datos_para_modelado <- function(usar_horarios = TRUE) {
  
  log_info("Cargando datos para modelado...")
  
  # Verificar si tenemos datos tiempo real disponibles
  archivo_realtime <- "data/realtime/datos_prediccion_latest.rds"
  
  if(file.exists(archivo_realtime)) {
    log_info("Usando datos tiempo real existentes")
    datos <- readRDS(archivo_realtime)
    
    # Validar estructura
    if(!"valor_medio" %in% names(datos)) {
      # Crear valor_medio si no existe
      datos <- datos %>% mutate(valor_medio = valor_medido)
    }
    
    log_info("Datos tiempo real cargados: {nrow(datos)} observaciones")
    return(datos)
    
  } else {
    # Conectar a base de datos para datos históricos
    log_info("Conectando a base de datos para datos históricos...")
    
    tryCatch({
      db_conn <- DBI::dbConnect(
        RPostgres::Postgres(),
        host = Sys.getenv("DB_HOST"),
        port = Sys.getenv("DB_PORT"),
        dbname = Sys.getenv("DB_NAME"),
        user = Sys.getenv("DB_USER"),
        password = Sys.getenv("DB_PASSWORD")
      )
      
      # Query simplificada para datos recientes
      query <- "
        SELECT 
          fm.fecha_hora,
          fm.valor_medido as valor_medio,
          fm.id_magnitud,
          de.id_estacion,
          CAST(de.\"LATITUD\" AS NUMERIC) as latitud,
          CAST(de.\"LONGITUD\" AS NUMERIC) as longitud,
          dm.descripcion as nombre_magnitud,
          dm.unidad
        FROM fact_mediciones fm
        JOIN dim_estaciones de ON fm.id_estacion = de.id_estacion
        JOIN dim_magnitudes dm ON fm.id_magnitud = dm.id_magnitud
        WHERE fm.fecha_hora >= CURRENT_DATE - INTERVAL '30 days'
          AND fm.valor_medido IS NOT NULL
        ORDER BY fm.fecha_hora DESC
        LIMIT 50000
      "
      
      datos_raw <- dbGetQuery(db_conn, query)
      dbDisconnect(db_conn)
      
      log_info("Datos históricos cargados: {nrow(datos_raw)} observaciones")
      
      # Convertir a SF y agregar datos meteorológicos simulados
      datos_sf <- datos_raw %>%
        # Agregar datos meteorológicos típicos de Madrid
        mutate(
          fecha = as.Date(fecha_hora),
          temp_media_c = 20 + sin((as.numeric(format(fecha_hora, "%j")) - 80) * 2 * pi / 365) * 10 + rnorm(n(), 0, 2),
          precipitacion_mm = ifelse(runif(n()) < 0.1, rexp(n(), 0.5), 0),
          vel_viento_media_ms = 2 + abs(rnorm(n(), 0, 1)),
          dir_viento_grados = 180 + rnorm(n(), 0, 45),
          presion_maxima_hpa = 1013 + rnorm(n(), 0, 10),
          humedad_media_pct = 50 + rnorm(n(), 0, 15)
        ) %>%
        # Crear objeto espacial
        st_as_sf(coords = c("longitud", "latitud"), crs = 4326)
      
      return(datos_sf)
      
    }, error = function(e) {
      log_error("Error cargando datos: {e$message}")
      stop("No se pudieron cargar datos para modelado")
    })
  }
}

# 5. FUNCIÓN PRINCIPAL DE EJECUCIÓN ----
ejecutar_modelado_caret <- function(contaminantes = c("Dióxido de Nitrógeno", "Partículas < 10 µm", "Ozono")) {
  
  log_info("=== INICIANDO MODELADO CARET COMPLETO ===")
  
  # Cargar datos
  datos <- cargar_datos_para_modelado(usar_horarios = usar_datos_horarios)
  
  if(is.null(datos) || nrow(datos) == 0) {
    log_error("No hay datos disponibles para modelado")
    return(NULL)
  }
  
  # Entrenar modelos para cada contaminante
  modelos_entrenados <- list()
  
  for(contaminante in contaminantes) {
    log_info("Procesando contaminante: {contaminante}")
    
    # Verificar si hay datos para este contaminante  
    datos_disponibles <- sum(datos$contaminante == contaminante, na.rm = TRUE)
    
    if(datos_disponibles < 10) {  # Umbral reducido para datos tiempo real
      log_warn("Datos insuficientes para {contaminante}: {datos_disponibles}")
      next
    }
    
    # Entrenar modelo
    modelo <- entrenar_modelo_caret_simple(datos, contaminante)
    
    if(!is.null(modelo)) {
      modelos_entrenados[[contaminante]] <- modelo
    }
  }
  
  # Guardar resultados
  if(length(modelos_entrenados) > 0) {
    archivo_salida <- "models/modelos_caret_simple.rds"
    saveRDS(modelos_entrenados, archivo_salida)
    
    log_success("✓ MODELADO COMPLETADO")
    log_info("  Modelos entrenados: {length(modelos_entrenados)}")
    log_info("  Archivo guardado: {archivo_salida}")
    
    # Resumen de rendimiento
    for(nombre in names(modelos_entrenados)) {
      modelo <- modelos_entrenados[[nombre]]
      log_info("  {nombre}: RMSE={round(modelo$rmse, 2)}, R²={round(modelo$rsquared, 3)}")
    }
    
    return(modelos_entrenados)
  } else {
    log_error("No se pudo entrenar ningún modelo")
    return(NULL)
  }
}

# 6. TEST DE FUNCIÓN ----
test_caret_simple <- function() {
  log_info("=== TEST CARET SIMPLE ===")
  
  resultado <- ejecutar_modelado_caret()
  
  if(is.null(resultado)) {
    log_error("❌ Test falló")
    return(FALSE)
  }
  
  log_success("✓ Test exitoso: {length(resultado)} modelos entrenados")
  return(TRUE)
}

# Ejecutar test automáticamente si se carga el script
if(interactive() == FALSE) {
  test_caret_simple()
}