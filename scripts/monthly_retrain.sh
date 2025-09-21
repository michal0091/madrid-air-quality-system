#!/bin/bash

# REENTRENAMIENTO MENSUAL EN RASPBERRY PI
# Ejecutar el primer día de cada mes para entrenar con datos verificados del mes anterior

# Configuración
PROJECT_DIR="/home/pi/madrid-air-quality-system"
LOG_DIR="$PROJECT_DIR/logs"
DATE=$(date +%Y%m%d)
LOG_FILE="$LOG_DIR/monthly_retrain_$DATE.log"
GITHUB_REPO="tu-usuario/madrid-air-quality-system"  # Actualizar con tu repo

# Crear directorio de logs
mkdir -p $LOG_DIR

# Función para logging
log_message() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a $LOG_FILE
}

# Función para verificar prerrequisitos
check_prerequisites() {
    log_message "🔍 Verificando prerrequisitos..."

    # Verificar espacio en disco (necesario ~5GB libres)
    AVAILABLE_SPACE=$(df $PROJECT_DIR | tail -1 | awk '{print $4}')
    REQUIRED_SPACE=5242880  # 5GB en KB

    if [ $AVAILABLE_SPACE -lt $REQUIRED_SPACE ]; then
        log_message "❌ Error: Espacio insuficiente. Disponible: $(($AVAILABLE_SPACE/1024/1024))GB, Requerido: 5GB"
        exit 1
    fi

    # Verificar conexión a BD
    if ! psql -h localhost -U air_quality_user -d air_quality_db -c "SELECT 1;" > /dev/null 2>&1; then
        log_message "❌ Error: No se puede conectar a la base de datos"
        exit 1
    fi

    # Verificar GitHub CLI
    if ! command -v gh &> /dev/null; then
        log_message "❌ Error: GitHub CLI no está instalado"
        exit 1
    fi

    log_message "✅ Prerrequisitos verificados"
}

# Función para calcular fechas de entrenamiento
calculate_training_dates() {
    # Calcular último día del mes anterior
    LAST_MONTH=$(date -d "$(date +%Y-%m-01) -1 day" +%Y-%m-%d)

    # Calcular fecha de inicio (10 años antes del último mes)
    START_DATE=$(date -d "$LAST_MONTH -10 years" +%Y-%m-%d)

    log_message "📅 Ventana de entrenamiento:"
    log_message "   Inicio: $START_DATE"
    log_message "   Fin: $LAST_MONTH"
    log_message "   Razón: Solo datos verificados (excluye mes actual)"
}

# Función para backup del modelo anterior
backup_previous_model() {
    log_message "💾 Creando backup del modelo anterior..."

    if [ -f "$PROJECT_DIR/models/modelos_espaciales.rds" ]; then
        cp "$PROJECT_DIR/models/modelos_espaciales.rds" \
           "$PROJECT_DIR/models/modelos_espaciales_backup_$DATE.rds"
        log_message "✅ Backup creado: modelos_espaciales_backup_$DATE.rds"
    else
        log_message "⚠️ No existe modelo anterior para hacer backup"
    fi
}

# Función para ejecutar entrenamiento
run_training() {
    log_message "🤖 Iniciando reentrenamiento mensual..."

    cd $PROJECT_DIR

    # Configurar fechas en variables de entorno
    export TRAINING_START_DATE="$START_DATE"
    export TRAINING_END_DATE="$LAST_MONTH"

    # Ejecutar entrenamiento
    Rscript R/02_train_spatial_models.R >> $LOG_FILE 2>&1

    if [ $? -eq 0 ]; then
        log_message "✅ Entrenamiento completado exitosamente"
        return 0
    else
        log_message "❌ Error en el entrenamiento"

        # Restaurar modelo anterior si existe
        if [ -f "$PROJECT_DIR/models/modelos_espaciales_backup_$DATE.rds" ]; then
            cp "$PROJECT_DIR/models/modelos_espaciales_backup_$DATE.rds" \
               "$PROJECT_DIR/models/modelos_espaciales.rds"
            log_message "🔄 Modelo anterior restaurado"
        fi

        return 1
    fi
}

# Función para validar modelo
validate_model() {
    log_message "🔍 Validando nuevo modelo..."

    # Verificar que el archivo existe y tiene tamaño razonable
    if [ ! -f "$PROJECT_DIR/models/modelos_espaciales.rds" ]; then
        log_message "❌ Error: Modelo no fue generado"
        return 1
    fi

    MODEL_SIZE=$(du -m "$PROJECT_DIR/models/modelos_espaciales.rds" | cut -f1)

    # Verificar tamaño (debe ser entre 100MB y 2GB)
    if [ $MODEL_SIZE -lt 100 ] || [ $MODEL_SIZE -gt 2048 ]; then
        log_message "❌ Error: Tamaño de modelo sospechoso: ${MODEL_SIZE}MB"
        return 1
    fi

    # Test rápido de carga del modelo
    Rscript -e "
    tryCatch({
        modelo <- readRDS('models/modelos_espaciales.rds')
        cat('✅ Modelo carga correctamente\n')
        if(is.list(modelo)) {
            cat('📊 Tipo: Lista con', length(modelo), 'elementos\n')
        }
    }, error = function(e) {
        cat('❌ Error cargando modelo:', e\$message, '\n')
        quit(status = 1)
    })
    " >> $LOG_FILE 2>&1

    if [ $? -eq 0 ]; then
        log_message "✅ Modelo validado correctamente (${MODEL_SIZE}MB)"
        return 0
    else
        log_message "❌ Error en validación del modelo"
        return 1
    fi
}

# Función para sincronizar con GitHub
sync_to_github() {
    log_message "📦 Sincronizando modelo con GitHub..."

    # Usar el script de sincronización
    $PROJECT_DIR/scripts/sync_model_to_github.sh 1 >> $LOG_FILE 2>&1

    if [ $? -eq 0 ]; then
        log_message "✅ Modelo sincronizado con GitHub"
        return 0
    else
        log_message "❌ Error sincronizando con GitHub"
        return 1
    fi
}

# Función para limpiar archivos temporales
cleanup() {
    log_message "🧹 Limpiando archivos temporales..."

    # Limpiar backups antiguos (mantener solo últimos 3 meses)
    find "$PROJECT_DIR/models" -name "modelos_espaciales_backup_*.rds" -mtime +90 -delete

    # Limpiar logs antiguos
    find "$LOG_DIR" -name "monthly_retrain_*.log" -mtime +180 -delete

    log_message "✅ Limpieza completada"
}

# EJECUCIÓN PRINCIPAL
log_message "🚀 INICIANDO REENTRENAMIENTO MENSUAL"
log_message "=============================================="

# Verificar que es el momento correcto (primeros 3 días del mes)
DAY_OF_MONTH=$(date +%d)
if [ $DAY_OF_MONTH -gt 3 ]; then
    log_message "⚠️ Advertencia: Ejecutando fuera de la ventana recomendada (días 1-3)"
fi

# Ejecutar pasos
check_prerequisites
calculate_training_dates
backup_previous_model

if run_training; then
    if validate_model; then
        if sync_to_github; then
            log_message "🎯 REENTRENAMIENTO MENSUAL COMPLETADO EXITOSAMENTE"
            cleanup

            # Resumen final
            log_message ""
            log_message "📊 RESUMEN:"
            log_message "   Modelo entrenado: $(date)"
            log_message "   Datos: $START_DATE a $LAST_MONTH"
            log_message "   Tamaño: $(du -h $PROJECT_DIR/models/modelos_espaciales.rds | cut -f1)"
            log_message "   Próximo entrenamiento: $(date -d 'next month' +%Y-%m-01)"

            exit 0
        else
            log_message "⚠️ Modelo entrenado pero no sincronizado"
            exit 2
        fi
    else
        log_message "❌ Modelo no válido"
        exit 3
    fi
else
    log_message "❌ Error en entrenamiento"
    exit 1
fi