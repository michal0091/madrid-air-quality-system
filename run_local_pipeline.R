#!/usr/bin/env Rscript
# LOCAL PIPELINE EQUIVALENTE A GITHUB ACTIONS
# ===========================================
# Replica exactamente el flujo de daily-predictions.yml para pruebas locales
# Usa datos REALES de AEMET y fallback para datos de contaminación
# Versión: 1.0.0
# Fecha: 2025-09-29

cat("🚀 === PIPELINE LOCAL MADRID AIR QUALITY === \n")
cat("Equivalente a GitHub Actions daily-predictions.yml\n\n")

# CONFIGURACIÓN INICIAL ====
library(logger)
log_threshold(INFO)

# Cargar .Renviron SIEMPRE (crítico para AEMET_API_KEY)
if(file.exists(".Renviron")) {
  readRenviron(".Renviron")
  cat("✅ .Renviron cargado - AEMET_API_KEY disponible\n")
} else {
  cat("⚠️ .Renviron no encontrado\n")
}

# Verificar API key
api_key <- Sys.getenv("AEMET_API_KEY")
if(nchar(api_key) > 0) {
  cat("✅ AEMET_API_KEY configurada (", nchar(api_key), " caracteres)\n")
} else {
  cat("❌ AEMET_API_KEY no configurada\n")
}

cat("\n")

# FUNCIONES AUXILIARES ====

#' Ejecuta un script con logging y manejo de errores
ejecutar_paso <- function(descripcion, script_code, obligatorio = TRUE) {
  cat("🔄", descripcion, "...\n")
  inicio <- Sys.time()

  tryCatch({
    eval(parse(text = script_code))
    tiempo <- round(as.numeric(difftime(Sys.time(), inicio, units = "secs")), 1)
    cat("✅", descripcion, "completado en", tiempo, "segundos\n")
    return(TRUE)
  }, error = function(e) {
    tiempo <- round(as.numeric(difftime(Sys.time(), inicio, units = "secs")), 1)
    cat("❌ Error en", descripcion, ":", e$message, "\n")
    if(obligatorio) {
      stop(paste("Paso obligatorio falló:", descripcion))
    }
    return(FALSE)
  })
}

#' Verificar que archivo existe y mostrar info
verificar_archivo <- function(path, descripcion) {
  if(file.exists(path)) {
    tamano <- round(file.size(path) / 1024 / 1024, 2)
    cat("✅", descripcion, ":", basename(path), "(", tamano, "MB)\n")
    return(TRUE)
  } else {
    cat("❌", descripcion, ":", basename(path), "NO ENCONTRADO\n")
    return(FALSE)
  }
}

# PIPELINE PRINCIPAL ====

cat("=== PASO 1: CREAR DIRECTORIOS ===\n")
ejecutar_paso("Crear directorios necesarios", "
  dirs <- c('data/realtime', 'output', 'logs', 'app/data', 'app/www/mapas_horas', 'app/www/horas')
  for(dir in dirs) {
    if(!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      cat('📁 Creado:', dir, '\n')
    }
  }
")

cat("\n=== PASO 2: VERIFICAR MODELO ===\n")
verificar_archivo("models/modelos_caret_avanzados.rds", "Modelo ML entrenado")

cat("\n=== PASO 3: OBTENER DATOS TIEMPO REAL ===\n")
ejecutar_paso("Predicción meteorológica AEMET", "
  source('R/meteo_forecast.R', local = TRUE)
", obligatorio = FALSE)

ejecutar_paso("Datos contaminación tiempo real", "
  source('R/datos_realtime_fallback.R', local = TRUE)
  datos_actuales <- obtener_datos_tiempo_real(usar_fallback = TRUE)
  saveRDS(datos_actuales, 'data/realtime/datos_prediccion_latest.rds')
  cat('📊 Datos tiempo real:', nrow(datos_actuales), 'registros\n')
")

cat("\n=== PASO 4: GENERAR PREDICCIONES ===\n")
ejecutar_paso("Predicciones horarias 40h", "
  source('R/07_predicciones_horarias.R', local = TRUE)
")

cat("\n=== PASO 5: GENERAR CONTENIDO DASHBOARD ===\n")
ejecutar_paso("Mapas por hora para animación", "
  if(file.exists('generar_mapas_por_hora.R')) {
    source('generar_mapas_por_hora.R', local = TRUE)
  } else {
    cat('⚠️ generar_mapas_por_hora.R no encontrado\n')
  }
", obligatorio = FALSE)

ejecutar_paso("Imágenes por hora", "
  if(file.exists('generar_imagenes_por_hora.R')) {
    source('generar_imagenes_por_hora.R', local = TRUE)
  } else {
    cat('⚠️ generar_imagenes_por_hora.R no encontrado\n')
  }
", obligatorio = FALSE)

cat("\n=== PASO 6: COPIAR DATOS A APP ===\n")
ejecutar_paso("Sincronizar datos con app", "
  # Copiar datos principales
  if(file.exists('output/predicciones_40h_latest.rds')) {
    file.copy('output/predicciones_40h_latest.rds', 'app/data/', overwrite = TRUE)
    cat('📄 predicciones_40h_latest.rds copiado\n')
  }

  if(file.exists('output/meteo_40h_latest.rds')) {
    file.copy('output/meteo_40h_latest.rds', 'app/data/', overwrite = TRUE)
    cat('📄 meteo_40h_latest.rds copiado\n')
  }
")

cat("\n=== VERIFICACIÓN FINAL ===\n")

# Verificar archivos críticos
archivos_criticos <- c(
  "output/predicciones_40h_latest.rds" = "Predicciones 40h",
  "output/meteo_40h_latest.rds" = "Datos meteorológicos 40h",
  "data/realtime/datos_prediccion_latest.rds" = "Datos tiempo real",
  "app/data/predicciones_40h_latest.rds" = "Predicciones en app",
  "app/data/meteo_40h_latest.rds" = "Meteorología en app"
)

cat("📋 Verificando archivos generados:\n")
for(path in names(archivos_criticos)) {
  verificar_archivo(path, archivos_criticos[path])
}

# Verificar mapas animados
cat("\n📊 Verificando mapas animados:\n")
mapas_dir <- "app/www/mapas_horas"
if(dir.exists(mapas_dir)) {
  mapas_png <- list.files(mapas_dir, pattern = "*.png", recursive = TRUE)
  cat("🗺️ Mapas PNG encontrados:", length(mapas_png), "\n")

  if(length(mapas_png) > 0) {
    # Mostrar algunos ejemplos
    ejemplos <- head(mapas_png, 3)
    for(mapa in ejemplos) {
      cat("  📄", mapa, "\n")
    }
    if(length(mapas_png) > 3) {
      cat("  ... y", length(mapas_png) - 3, "más\n")
    }
  }
} else {
  cat("❌ Directorio de mapas no existe\n")
}

# Verificar datos para Shiny
cat("\n📊 Resumen datos para Shiny:\n")
if(file.exists("app/data/predicciones_40h_latest.rds")) {
  pred <- readRDS("app/data/predicciones_40h_latest.rds")
  cat("✅ Predicciones cargadas:", nrow(pred), "registros\n")
  cat("  📅 Horas únicas:", length(unique(pred$fecha_hora)), "\n")
  cat("  🏭 Estaciones:", length(unique(pred$id_estacion)), "\n")
  cat("  🌬️ Contaminantes:", length(unique(pred$contaminante)), "\n")
  cat("  📊 Rango valores:", round(min(pred$prediccion, na.rm = TRUE), 1), "-",
      round(max(pred$prediccion, na.rm = TRUE), 1), "µg/m³\n")
}

cat("\n🎉 === PIPELINE LOCAL COMPLETADO ===\n")
cat("🚀 Para lanzar la app localmente:\n")
cat("   # La app Shiny está en app/ - usa:\n")
cat("   shiny::runApp('app', port = 3838)\n")
cat("\n📱 La app estará disponible en: http://localhost:3838\n")