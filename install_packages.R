#!/usr/bin/env Rscript
# Install critical packages for Madrid Air Quality System (XGBoost version)

cat("📦 Instalando paquetes críticos para predicciones con XGBoost...\n\n")

# Repositorio CRAN
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Paquetes críticos en orden de dependencia
critical_packages <- c(
  # Core R utilities
  "data.table",
  "lubridate",
  "logger",
  "glue",

  # Machine Learning (XGBoost - NUEVO)
  "xgboost",

  # Web/API
  "httr2",
  "rvest",
  "jsonlite",
  "xml2",

  # Database
  "DBI",
  "RPostgres",

  # Spatial
  "sf",
  "lwgeom",

  # Visualization (para generar mapas)
  "ggplot2",
  "ggrepel",
  "mapSpain",
  "tidyterra",

  # Shiny (si se ejecuta dashboard)
  "shiny",
  "shinydashboard",
  "leaflet",
  "plotly",
  "DT"
)

cat("📋 Instalando", length(critical_packages), "paquetes...\n")

# Instalar paquetes que no estén instalados
for (pkg in critical_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("  📥 Instalando:", pkg, "\n")
    install.packages(pkg, dependencies = TRUE, quiet = FALSE)
  } else {
    cat("  ✅ Ya instalado:", pkg, "\n")
  }
}

# Verificar instalación crítica
cat("\n🔍 Verificando paquetes críticos...\n")
critical_core <- c("xgboost", "data.table", "sf", "logger", "xml2")

all_ok <- TRUE
for (pkg in critical_core) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("  ✅", pkg, "\n")
  } else {
    cat("  ❌ FALTANTE:", pkg, "\n")
    all_ok <- FALSE
  }
}

if (all_ok) {
  cat("\n✅ Todos los paquetes críticos instalados correctamente\n")
} else {
  stop("❌ Error: Paquetes críticos faltantes")
}
