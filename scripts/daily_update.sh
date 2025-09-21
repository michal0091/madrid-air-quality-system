#!/bin/bash

# MADRID AIR QUALITY - SCRIPT DE ACTUALIZACIÓN DIARIA
# Ejecuta a la 1:00 AM todos los días
# Autor: Sistema automatizado

# Configuración
PROJECT_DIR="/home/pi/madrid-air-quality-system"
LOG_DIR="$PROJECT_DIR/logs"
LOG_FILE="$LOG_DIR/daily_update_$(date +%Y%m%d).log"
R_CMD="Rscript"

# Crear directorio de logs si no existe
mkdir -p $LOG_DIR

# Función para logging
log_message() {
    echo "$(date '+%Y-%m-%d %H:%M:%S') - $1" | tee -a $LOG_FILE
}

# Función para ejecutar scripts R con manejo de errores
run_r_script() {
    local script_path="$1"
    local script_name=$(basename "$script_path")
    
    log_message "Iniciando ejecución de $script_name"
    
    cd $PROJECT_DIR
    $R_CMD "$script_path" >> $LOG_FILE 2>&1
    
    if [ $? -eq 0 ]; then
        log_message "✅ $script_name completado exitosamente"
        return 0
    else
        log_message "❌ Error en $script_name"
        return 1
    fi
}

# Iniciar proceso de actualización
log_message "🚀 INICIANDO ACTUALIZACIÓN DIARIA DE DATOS"

# Paso 1: Recolectar datos históricos (si es necesario)
# Comentado para evitar ejecución diaria pesada
# run_r_script "R/01b_collect_historical_data.R"

# Paso 2: Crear predictores actualizados
if run_r_script "R/01c_create_predictors.R"; then
    log_message "Predictores actualizados correctamente"
else
    log_message "Error actualizando predictores - continuando..."
fi

# Paso 3: Recolectar datos meteorológicos
if run_r_script "R/01d_collect_meteo_data.R"; then
    log_message "Datos meteorológicos actualizados"
else
    log_message "Error en datos meteorológicos - continuando..."
fi

# Paso 4: Entrenar modelos espaciales (cada 7 días para evitar sobrecarga)
DAYOFWEEK=$(date +%u)
if [ $DAYOFWEEK -eq 1 ]; then  # Lunes
    log_message "Es lunes - entrenando modelos espaciales..."
    if run_r_script "R/02_train_spatial_models.R"; then
        log_message "Modelos espaciales entrenados correctamente"
    else
        log_message "Error entrenando modelos - usando modelos existentes"
    fi
else
    log_message "No es lunes - saltando entrenamiento de modelos"
fi

# Paso 5: Preparar datos en tiempo real
if run_r_script "R/03_prepare_realtime_prediction.R"; then
    log_message "Datos en tiempo real preparados"
else
    log_message "Error preparando datos en tiempo real"
fi

# Paso 6: Ejecutar predicciones
if run_r_script "R/03b_execute_predictions.R"; then
    log_message "Predicciones ejecutadas correctamente"
else
    log_message "Error ejecutando predicciones"
fi

# Paso 7: Generar mapas por hora (solo los más recientes)
if run_r_script "generar_mapas_por_hora.R"; then
    log_message "Mapas por hora generados"
else
    log_message "Error generando mapas - usando mapas existentes"
fi

# Paso 8: Limpiar archivos antiguos (mantener solo últimos 7 días)
log_message "Limpiando archivos antiguos..."
find $LOG_DIR -name "*.log" -mtime +7 -delete
find $PROJECT_DIR/data -name "*_old_*" -mtime +7 -delete

# Paso 9: Reiniciar Shiny Server para cargar nuevos datos
log_message "Reiniciando Shiny Server..."
sudo systemctl restart shiny-server

if [ $? -eq 0 ]; then
    log_message "✅ Shiny Server reiniciado correctamente"
else
    log_message "⚠️ Error reiniciando Shiny Server"
fi

# Resumen final
log_message "🎯 ACTUALIZACIÓN DIARIA COMPLETADA"
log_message "Próxima ejecución: $(date -d '+1 day' '+%Y-%m-%d 01:00:00')"

# Opcional: Enviar notificación por email (si está configurado)
# echo "Actualización diaria completada en $(hostname)" | mail -s "Madrid Air Quality Update" admin@domain.com

exit 0