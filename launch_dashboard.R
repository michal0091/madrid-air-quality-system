#!/usr/bin/env Rscript

# launch_dashboard.R
# Script para lanzar el dashboard de calidad del aire de Madrid en local
#
# Uso: Rscript launch_dashboard.R
# O desde R: source("launch_dashboard.R")

# Verificar que estamos en el directorio correcto
if(!file.exists("app/app.R")) {
  stop("❌ Este script debe ejecutarse desde el directorio raíz del proyecto")
}

# Verificar que existen los datos necesarios
archivos_necesarios <- c(
  "app/data/predicciones_40h_latest.rds",
  "app/data/meteo_40h_latest.rds"
)

for(archivo in archivos_necesarios) {
  if(!file.exists(archivo)) {
    cat("⚠️ Archivo faltante:", archivo, "\n")
    cat("💡 Ejecuta primero: source('run_local_pipeline.R')\n")
    stop("❌ Datos no disponibles")
  }
}

# Cargar librerías necesarias para Shiny
library(shiny)

cat("🚀 Lanzando dashboard de calidad del aire de Madrid...\n")
cat("📊 Datos actualizados:", format(file.mtime("app/data/predicciones_40h_latest.rds")), "\n")
cat("🌐 El dashboard se abrirá en tu navegador predeterminado\n")
cat("🛑 Para detener el servidor: Ctrl+C\n\n")

# Cambiar al directorio app y lanzar
setwd("app")
runApp(port = 3838, launch.browser = TRUE)