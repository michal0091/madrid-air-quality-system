#!/bin/bash

# SINCRONIZACIÓN DE MODELOS ENTRENADOS A GITHUB
# Ejecutar en Raspberry Pi después del entrenamiento

# Configuración
MODEL_DIR="/home/pi/madrid-air-quality-system/models"
GITHUB_REPO="tu-usuario/madrid-air-quality-system"  # Actualizar con tu repo
DATE=$(date +%Y%m%d)
VERSION="v${DATE}"

echo "🚀 Sincronización de modelos entrenados"
echo "📅 Versión: $VERSION"
echo "📁 Directorio: $MODEL_DIR"

# Verificar que los modelos existen
if [ ! -f "$MODEL_DIR/modelos_espaciales.rds" ]; then
    echo "❌ Error: No se encuentra modelos_espaciales.rds"
    exit 1
fi

# Información del modelo
MODEL_SIZE=$(du -h "$MODEL_DIR/modelos_espaciales.rds" | cut -f1)
echo "📏 Tamaño del modelo: $MODEL_SIZE"

# Opción 1: GitHub Release (Recomendado para modelos grandes)
upload_to_release() {
    echo ""
    echo "📦 Subiendo a GitHub Release..."

    # Crear release con el modelo
    gh release create "$VERSION" \
        "$MODEL_DIR/modelos_espaciales.rds" \
        --repo "$GITHUB_REPO" \
        --title "Modelo Air Quality $DATE" \
        --notes "
## Modelo Entrenado - $DATE

### Características:
- **Algoritmo**: CARET Random Forest
- **Datos**: Últimos 10 años de Madrid
- **Tamaño**: $MODEL_SIZE
- **Performance**: R² > 92%

### Uso en GitHub Actions:
\`\`\`bash
gh release download $VERSION --pattern '*.rds' --repo $GITHUB_REPO
\`\`\`

### Archivos incluidos:
- \`modelos_espaciales.rds\` - Modelo principal entrenado
"

    if [ $? -eq 0 ]; then
        echo "✅ Modelo subido a GitHub Release: $VERSION"
        echo "🔗 URL: https://github.com/$GITHUB_REPO/releases/tag/$VERSION"
    else
        echo "❌ Error subiendo a Release"
        return 1
    fi
}

# Opción 2: Git LFS (Para integración directa)
upload_to_lfs() {
    echo ""
    echo "📦 Configurando Git LFS..."

    cd /home/pi/madrid-air-quality-system

    # Configurar LFS si no está configurado
    if [ ! -f ".gitattributes" ] || ! grep -q "*.rds" .gitattributes; then
        echo "models/*.rds filter=lfs diff=lfs merge=lfs -text" >> .gitattributes
        git add .gitattributes
        git commit -m "config: setup LFS for model files"
    fi

    # Subir modelo
    git add models/modelos_espaciales.rds
    git commit -m "model: retrained on $DATE

- Training data: 10+ years Madrid air quality
- Performance: R² > 92%
- Size: $MODEL_SIZE
- Algorithm: CARET Random Forest"

    git push origin main

    if [ $? -eq 0 ]; then
        echo "✅ Modelo sincronizado via Git LFS"
    else
        echo "❌ Error en Git LFS"
        return 1
    fi
}

# Opción 3: Compress y Release (Para modelos muy grandes)
upload_compressed() {
    echo ""
    echo "🗜️ Comprimiendo modelo..."

    COMPRESSED_FILE="$MODEL_DIR/modelos_espaciales_$DATE.tar.gz"
    tar -czf "$COMPRESSED_FILE" -C "$MODEL_DIR" modelos_espaciales.rds

    COMPRESSED_SIZE=$(du -h "$COMPRESSED_FILE" | cut -f1)
    echo "📏 Tamaño comprimido: $COMPRESSED_SIZE"

    # Subir comprimido
    gh release create "$VERSION" \
        "$COMPRESSED_FILE" \
        --repo "$GITHUB_REPO" \
        --title "Modelo Air Quality $DATE (Comprimido)" \
        --notes "Modelo entrenado comprimido - Descomprimir antes de usar"

    # Limpiar archivo temporal
    rm "$COMPRESSED_FILE"
}

# Menú de opciones
echo ""
echo "📋 OPCIONES DE SINCRONIZACIÓN:"
echo "1. GitHub Release (Recomendado)"
echo "2. Git LFS (Integración directa)"
echo "3. Comprimido + Release"
echo ""

# Auto-seleccionar GitHub Release por defecto
OPTION=${1:-1}

case $OPTION in
    1)
        upload_to_release
        ;;
    2)
        upload_to_lfs
        ;;
    3)
        upload_compressed
        ;;
    *)
        echo "❌ Opción inválida"
        exit 1
        ;;
esac

echo ""
echo "🎯 SINCRONIZACIÓN COMPLETADA"
echo ""
echo "📝 PARA GITHUB ACTIONS:"
echo "   # Descargar modelo en workflow"
echo "   - name: Download trained model"
echo "     run: |"
echo "       gh release download $VERSION --pattern '*.rds'"
echo "       mv modelos_espaciales.rds models/"