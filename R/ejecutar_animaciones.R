# EJECUTAR GENERACIÓN DE ANIMACIONES
# Script simple para generar las animaciones offline

# Cargar librerías principales
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(gganimate)
})

setwd("/mnt/c/Users/miki/code/madrid-air-quality-system")
source("R/06_generar_animaciones.R")

# Ejecutar generación con datos de prueba inicialmente
cat("Iniciando generación de animaciones...\n")
cat("Probando primero con datos sintéticos...\n")

resultado <- generar_todas_animaciones(usar_datos_prueba = TRUE)

if(resultado) {
  cat("\n🎉 ¡LISTO! Las animaciones han sido generadas.\n")
  cat("📁 Archivos generados en: app/www/\n")
  cat("Ahora puede ejecutar la aplicación Shiny.\n")
  
  # Listar archivos generados
  archivos_gif <- list.files("app/www", pattern = "\\.gif$", full.names = FALSE)
  if(length(archivos_gif) > 0) {
    cat("📄 Archivos GIF creados:\n")
    for(archivo in archivos_gif) {
      info <- file.info(file.path("app/www", archivo))
      tamaño_mb <- round(info$size / 1024^2, 2)
      cat("  -", archivo, "(", tamaño_mb, "MB )\n")
    }
  }
} else {
  cat("\n❌ Hubo problemas generando las animaciones.\n")
  cat("Revise los mensajes de error anteriores.\n")
}