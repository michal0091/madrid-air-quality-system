# Script para instalar todos los paquetes R necesarios
# Usado en Dockerfile para pre-instalar dependencias

cat("=== INSTALANDO PAQUETES R ===\n\n")

# Paquetes esenciales para predicciones
packages <- c(
  'DBI', 'RPostgres', 'caret', 'randomForest', 'ranger', 'sf',
  'dplyr', 'logger', 'httr2', 'jsonlite', 'lubridate',
  'data.table', 'shiny', 'leaflet', 'plotly', 'ggplot2',
  'rsconnect', 'shinydashboard', 'DT', 'htmltools',
  'shinycssloaders', 'tidyr', 'viridis', 'yaml',
  'ggrepel', 'purrr', 'gganimate', 'transformr', 'xml2'
)

# Paquetes adicionales para generación de mapas
map_packages <- c('tidyterra', 'mapSpain', 'slippymath')

# Función para instalar con reintentos
install_with_retry <- function(pkg, max_attempts = 3) {
  for(attempt in 1:max_attempts) {
    tryCatch({
      if (!requireNamespace(pkg, quietly = TRUE)) {
        install.packages(pkg, dependencies = TRUE)
        cat('✅ Instalado:', pkg, '\n')
      } else {
        cat('⚡ Ya disponible:', pkg, '\n')
      }
      return(TRUE)
    }, error = function(e) {
      if(attempt < max_attempts) {
        cat('⚠️ Intento', attempt, 'falló para', pkg, '- reintentando...\n')
        Sys.sleep(2)
      } else {
        cat('❌ ERROR instalando', pkg, ':', e$message, '\n')
        return(FALSE)
      }
    })
  }
}

# Instalar paquetes principales
cat("\n📦 Instalando paquetes principales...\n")
for(pkg in packages) {
  install_with_retry(pkg)
}

# Instalar paquetes de mapas (críticos)
cat("\n🗺️ Instalando paquetes de mapas...\n")

# mapSpain (CRÍTICO)
tryCatch({
  if (!requireNamespace('mapSpain', quietly = TRUE)) {
    install.packages('mapSpain')
    cat('✅ mapSpain instalado - CRÍTICO para madrid_mask\n')
  } else {
    cat('⚡ mapSpain ya disponible\n')
  }
}, error = function(e) {
  cat('❌ ERROR CRÍTICO: No se pudo instalar mapSpain:', e$message, '\n')
})

# tidyterra
tryCatch({
  if (!requireNamespace('tidyterra', quietly = TRUE)) {
    install.packages('tidyterra')
    cat('✅ tidyterra instalado\n')
  } else {
    cat('⚡ tidyterra ya disponible\n')
  }
}, error = function(e) {
  cat('⚠️ No se pudo instalar tidyterra:', e$message, '\n')
})

# slippymath
tryCatch({
  if (!requireNamespace('slippymath', quietly = TRUE)) {
    install.packages('slippymath')
    cat('✅ slippymath instalado\n')
  } else {
    cat('⚡ slippymath ya disponible\n')
  }
}, error = function(e) {
  cat('⚠️ No se pudo instalar slippymath:', e$message, '\n')
})

# Verificación final
cat("\n=== VERIFICACIÓN FINAL ===\n")
required_critical <- c('ranger', 'sf', 'caret', 'dplyr', 'logger', 'xml2', 'mapSpain')

all_installed <- TRUE
for(pkg in required_critical) {
  if(requireNamespace(pkg, quietly = TRUE)) {
    cat('✅', pkg, '\n')
  } else {
    cat('❌ FALTANTE:', pkg, '\n')
    all_installed <- FALSE
  }
}

if(all_installed) {
  cat('\n🎉 TODOS LOS PAQUETES CRÍTICOS INSTALADOS EXITOSAMENTE\n')
} else {
  stop('❌ Faltan paquetes críticos')
}

cat('\n✅ Instalación completada\n')
