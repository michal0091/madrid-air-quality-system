#!/bin/bash
# Script para construir y probar Docker image localmente

set -e  # Exit on error

echo "🐳 ============================================"
echo "   CONSTRUCCIÓN DOCKER - Madrid Air Quality"
echo "============================================"

# Colores
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # No Color

IMAGE_NAME="madrid-air-quality-local"
GHCR_IMAGE="ghcr.io/michal0091/madrid-air-quality-system/air-quality-predictor"

# Función para mostrar mensajes
log_info() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

log_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

log_error() {
    echo -e "${RED}❌ $1${NC}"
}

# Opción 1: Construir localmente
build_local() {
    log_info "Construyendo imagen Docker localmente..."
    log_info "Esto puede tardar 15-20 minutos la primera vez"

    docker build -t $IMAGE_NAME . || {
        log_error "Error construyendo imagen"
        exit 1
    }

    log_success "Imagen construida: $IMAGE_NAME"

    # Mostrar tamaño
    SIZE=$(docker images $IMAGE_NAME --format "{{.Size}}")
    log_info "Tamaño de imagen: $SIZE"
}

# Opción 2: Pull desde GitHub Container Registry
pull_ghcr() {
    log_info "Descargando imagen desde GitHub Container Registry..."

    docker pull $GHCR_IMAGE:latest || {
        log_error "Error descargando imagen. ¿Está publicada en GHCR?"
        exit 1
    }

    log_success "Imagen descargada: $GHCR_IMAGE:latest"
}

# Opción 3: Verificar paquetes
verify_packages() {
    log_info "Verificando paquetes instalados..."

    docker run --rm $IMAGE_NAME Rscript -e "
    critical <- c('ranger', 'sf', 'caret', 'dplyr', 'logger', 'xml2', 'mapSpain')
    cat('\n=== PAQUETES CRÍTICOS ===\n')
    for(pkg in critical) {
      if(requireNamespace(pkg, quietly=TRUE)) {
        cat('✅', pkg, as.character(packageVersion(pkg)), '\n')
      } else {
        cat('❌', pkg, 'FALTANTE\n')
      }
    }
    cat('\n')
    " || {
        log_error "Error verificando paquetes"
        exit 1
    }

    log_success "Verificación completada"
}

# Opción 4: Test interactivo
test_interactive() {
    log_info "Iniciando R interactivo en contenedor..."
    log_info "Directorio actual montado en /app"

    docker run --rm -it \
        -v "$(pwd):/app" \
        -w /app \
        $IMAGE_NAME \
        R
}

# Opción 5: Test predicción
test_prediction() {
    log_info "Probando script de predicción..."

    if [ ! -f "data/realtime/prediccion_meteo_latest.rds" ]; then
        log_error "No existen datos de entrada. Ejecuta primero la recolección de datos."
        exit 1
    fi

    docker run --rm \
        -v "$(pwd):/app" \
        -w /app \
        $IMAGE_NAME \
        Rscript R/05_predicciones_horarias.R

    log_success "Test de predicción completado"
}

# Menú principal
show_menu() {
    echo ""
    echo "Selecciona una opción:"
    echo "  1) Construir imagen localmente"
    echo "  2) Descargar imagen desde GHCR"
    echo "  3) Verificar paquetes instalados"
    echo "  4) Abrir R interactivo"
    echo "  5) Probar script de predicción"
    echo "  6) Limpiar imágenes antiguas"
    echo "  0) Salir"
    echo ""
}

# Limpiar
clean_images() {
    log_info "Limpiando imágenes Docker antiguas..."
    docker system prune -f
    log_success "Limpieza completada"
}

# Loop del menú
while true; do
    show_menu
    read -p "Opción: " choice

    case $choice in
        1)
            build_local
            verify_packages
            ;;
        2)
            pull_ghcr
            verify_packages
            ;;
        3)
            verify_packages
            ;;
        4)
            test_interactive
            ;;
        5)
            test_prediction
            ;;
        6)
            clean_images
            ;;
        0)
            log_info "Saliendo..."
            exit 0
            ;;
        *)
            log_error "Opción inválida"
            ;;
    esac
done
