# MAIN.R - SCRIPT MAESTRO DEL SISTEMA DE CALIDAD DEL AIRE MADRID
# =================================================================
# Orquesta todas las fases del pipeline de datos y predicciones
# Versión: 1.0.0
# Fecha: 2025-09-02

# CONFIGURACIÓN GLOBAL ====
library(logger)
library(lubridate)
library(dplyr)

# Setup logging
log_threshold(INFO)
log_appender(appender_file("logs/main_pipeline.log"))
log_info("=== INICIO PIPELINE MAESTRO - {Sys.time()} ===")

# Parámetros globales
FORZAR_SETUP <- FALSE           # Recrear tablas dimensión
CARGAR_HISTORICOS <- FALSE      # Cargar datos históricos (proceso intensivo)
CREAR_MODELOS <- FALSE          # Entrenar modelos ML (proceso intensivo)
EJECUTAR_PREDICCIONES <- TRUE   # Generar predicciones actuales
USAR_FALLBACK <- FALSE          # Usar datos simulados si APIs fallan
LANZAR_DASHBOARD <- TRUE       # Ejecutar dashboard Shiny

# FUNCIONES AUXILIARES ====

#' Ejecuta un script R con manejo de errores y logging
ejecutar_script <- function(script_path, descripcion, obligatorio = TRUE) {
  log_info("🔄 Ejecutando: {descripcion}")
  inicio <- Sys.time()
  
  tryCatch({
    source(script_path, local = TRUE)
    tiempo <- round(as.numeric(difftime(Sys.time(), inicio, units = "secs")), 1)
    log_success("✅ {descripcion} completado en {tiempo}s")
    return(TRUE)
  }, error = function(e) {
    tiempo <- round(as.numeric(difftime(Sys.time(), inicio, units = "secs")), 1)
    log_error("❌ Error en {descripcion}: {e$message}")
    if(obligatorio) {
      stop(paste("Script obligatorio falló:", descripcion))
    }
    return(FALSE)
  })
}

#' Verifica existencia de archivos requeridos
verificar_prerequisitos <- function() {
  log_info("🔍 Verificando prerequisitos...")
  
  # Verificar conexión BD (opcional para desarrollo)
  tryCatch({
    source("R/utils.R", local = TRUE)
    conn <- get_db_connection()
    DBI::dbDisconnect(conn)
    log_info("✅ Conexión base de datos OK")
  }, error = function(e) {
    log_warn("⚠️ Sin conexión BD: {e$message}")
    log_info("💡 Continuando en modo desarrollo sin BD...")
  })
  
  # Verificar directorios
  dirs <- c("logs", "models", "output", "data/realtime")
  for(dir in dirs) {
    if(!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      log_info("📁 Creado directorio: {dir}")
    }
  }
  
  log_success("✅ Prerequisitos verificados")
}

# PIPELINE PRINCIPAL ====

main <- function() {
  log_info("🚀 INICIANDO PIPELINE MAESTRO")
  
  # Verificar prerequisitos
  verificar_prerequisitos()
  
  # FASE 1: SETUP INICIAL ----
  if(FORZAR_SETUP) {
    log_info("📊 === FASE 1: SETUP TABLAS DIMENSIÓN ===")
    ejecutar_script("R/00_setup_dimension_tables.R", 
                   "Setup tablas dimensión", obligatorio = TRUE)
  }
  
  # FASE 2: CARGA DATOS HISTÓRICOS ----
  if(CARGAR_HISTORICOS) {
    log_info("📈 === FASE 2: CARGA DATOS HISTÓRICOS ===")
    ejecutar_script("R/01b_collect_historical_data.R", 
                   "Carga datos históricos", obligatorio = FALSE)
    ejecutar_script("R/01c_create_predictors.R", 
                   "Creación predictores", obligatorio = FALSE)
    ejecutar_script("R/01d_collect_meteo_data.R", 
                   "Carga datos meteorológicos", obligatorio = FALSE)
  }
  
  # FASE 3: MODELADO ----
  if(CREAR_MODELOS) {
    log_info("🤖 === FASE 3: ENTRENAMIENTO MODELOS ===")
    ejecutar_script("R/02_modelo_caret_avanzado.R", 
                   "Modelado CARET Avanzado (10 años datos)", obligatorio = FALSE)
    ejecutar_script("R/03_prediccion_espacial.R", 
                   "Modelado espacial", obligatorio = FALSE)
  }
  
  # FASE 4: PREDICCIONES TIEMPO REAL ----
  if(EJECUTAR_PREDICCIONES) {
    log_info("⏱️ === FASE 4: PREDICCIONES TIEMPO REAL ===")
    
    # Obtener predicción meteorológica
    ejecutar_script("R/meteo_forecast.R", 
                   "Predicción meteorológica AEMET", obligatorio = FALSE)
    
    # Recolectar datos actuales
    if(USAR_FALLBACK) {
      log_info("⚠️ Usando sistema fallback para datos actuales")
      source("R/datos_realtime_fallback.R", local = TRUE)
      datos_actuales <- obtener_datos_tiempo_real(usar_fallback = TRUE)
      saveRDS(datos_actuales, "data/realtime/datos_prediccion_latest.rds")
      log_success("✅ Datos actuales preparados")
    }
    
    # Generar predicciones temporales (consolida mapas y gráficos)
    ejecutar_script("R/05_predicciones_horarias.R", 
                   "Predicciones horarias 40h", obligatorio = FALSE)
  }
  
  # FASE 5: DASHBOARD ----
  if(LANZAR_DASHBOARD) {
    log_info("📊 === FASE 5: DASHBOARD INTERACTIVO ===")
    source("R/08_dashboard_shiny.R", local = TRUE)
    ejecutar_dashboard(puerto = 3838)
  }
  
  # RESUMEN FINAL ----
  log_info("📋 === RESUMEN EJECUCIÓN ===")
  
  # Verificar outputs generados
  outputs <- c(
    "models/modelos_caret_avanzados.rds" = "Modelos ML entrenados",
    "output/predicciones_40h_latest.rds" = "Predicciones temporales 40h",
    "output/meteo_40h_latest.rds" = "Datos meteorológicos 40h",
    "data/realtime/datos_prediccion_latest.rds" = "Datos tiempo real",
    "data/realtime/prediccion_meteo_latest.rds" = "Predicción meteorológica"
  )
  
  for(path in names(outputs)) {
    if(file.exists(path) || dir.exists(path)) {
      log_info("✅ {outputs[path]}: Disponible")
    } else {
      log_warn("⚠️ {outputs[path]}: No encontrado")
    }
  }
  
  tiempo_total <- round(as.numeric(difftime(Sys.time(), inicio_global, units = "mins")), 1)
  log_success("🎉 PIPELINE COMPLETADO en {tiempo_total} minutos")
}

# EJECUCIÓN ====
# Variables globales para tiempo
inicio_global <- Sys.time()

# Ejecutar automáticamente según configuración
if(!interactive()) {
  # Script ejecutado desde línea de comandos
  args <- commandArgs(trailingOnly = TRUE)
  
  if(length(args) > 0) {
    # Parsear argumentos de línea de comandos
    if("--setup" %in% args) FORZAR_SETUP <- TRUE
    if("--historicos" %in% args) CARGAR_HISTORICOS <- TRUE
    if("--modelos" %in% args) CREAR_MODELOS <- TRUE
    if("--predicciones" %in% args) EJECUTAR_PREDICCIONES <- TRUE
    if("--dashboard" %in% args) LANZAR_DASHBOARD <- TRUE
    if("--no-fallback" %in% args) USAR_FALLBACK <- FALSE
  }
  
  # Ejecutar pipeline automáticamente
  main()
} else {
  # En modo interactivo, ejecutar según parámetros globales configurados
  log_info("📋 main.R cargado en modo interactivo")
  
  # Verificar si hay alguna fase activa
  fases_activas <- c(FORZAR_SETUP, CARGAR_HISTORICOS, CREAR_MODELOS, 
                    EJECUTAR_PREDICCIONES, LANZAR_DASHBOARD)
  
  if(any(fases_activas)) {
    log_info("⚡ Ejecutando pipeline automáticamente según configuración...")
    main()
  } else {
    log_info("💤 Todas las fases desactivadas. Use main() para ejecutar manualmente.")
    log_info("💡 O modifique los parámetros globales y vuelva a cargar el script.")
  }
}
