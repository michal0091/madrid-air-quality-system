#!/bin/bash

# CONFIGURAR SISTEMA DE LOGGING DE PREDICCIONES
# Inicializar tabla y funciones para análisis retrospectivo

# Configuración
PROJECT_DIR="/home/pi/madrid-air-quality-system"
DB_NAME="air_quality_db"
DB_USER="air_quality_user"
SQL_FILE="$PROJECT_DIR/scripts/create_prediction_log_table.sql"

echo "🚀 Configurando sistema de logging de predicciones"
echo "📊 Base de datos: $DB_NAME"

# Verificar que el archivo SQL existe
if [ ! -f "$SQL_FILE" ]; then
    echo "❌ Error: No se encuentra $SQL_FILE"
    exit 1
fi

# Ejecutar script SQL
echo "📝 Creando tabla de log de predicciones..."
psql -h localhost -U $DB_USER -d $DB_NAME -f "$SQL_FILE"

if [ $? -eq 0 ]; then
    echo "✅ Tabla de log creada correctamente"
else
    echo "❌ Error creando tabla de log"
    exit 1
fi

# Verificar que la tabla se creó
echo "🔍 Verificando estructura de la tabla..."
TABLE_EXISTS=$(psql -h localhost -U $DB_USER -d $DB_NAME -t -c "
SELECT EXISTS (
    SELECT FROM information_schema.tables
    WHERE table_schema = 'public'
    AND table_name = 'log_predicciones'
);")

if [[ "$TABLE_EXISTS" == *"t"* ]]; then
    echo "✅ Tabla log_predicciones verificada"

    # Mostrar estructura
    echo ""
    echo "📋 ESTRUCTURA DE LA TABLA:"
    psql -h localhost -U $DB_USER -d $DB_NAME -c "\d log_predicciones"

    # Verificar vistas
    echo ""
    echo "📊 VISTAS CREADAS:"
    psql -h localhost -U $DB_USER -d $DB_NAME -c "
    SELECT viewname, definition
    FROM pg_views
    WHERE schemaname = 'public'
    AND viewname LIKE '%performance%' OR viewname LIKE '%monitoreo%';
    "

else
    echo "❌ Error: Tabla no fue creada correctamente"
    exit 1
fi

# Probar funciones R
echo ""
echo "🧪 Probando funciones R de logging..."
cd $PROJECT_DIR

Rscript -e "
source('R/prediction_logging.R')

# Test básico de conexión
tryCatch({
    con <- conectar_bd()

    # Verificar que la tabla existe
    tablas <- DBI::dbListTables(con)
    if ('log_predicciones' %in% tablas) {
        cat('✅ Tabla log_predicciones accesible desde R\n')

        # Contar registros actuales
        count <- DBI::dbGetQuery(con, 'SELECT COUNT(*) as total FROM log_predicciones')
        cat('📊 Registros actuales en log:', count\$total, '\n')

    } else {
        cat('❌ Error: Tabla no accesible desde R\n')
        quit(status = 1)
    }

    DBI::dbDisconnect(con)

}, error = function(e) {
    cat('❌ Error en test R:', e\$message, '\n')
    quit(status = 1)
})

cat('✅ Funciones R funcionando correctamente\n')
"

if [ $? -eq 0 ]; then
    echo "✅ Test de funciones R completado"
else
    echo "❌ Error en test de funciones R"
    exit 1
fi

echo ""
echo "🎯 SISTEMA DE LOGGING CONFIGURADO"
echo ""
echo "📋 CARACTERÍSTICAS:"
echo "   ✅ Tabla log_predicciones creada"
echo "   ✅ Vistas de análisis configuradas"
echo "   ✅ Función de actualización automática"
echo "   ✅ Funciones R de logging listas"
echo ""
echo "🔄 USO:"
echo "   # En scripts de predicción:"
echo "   source('R/prediction_logging.R')"
echo "   guardar_predicciones_log(predicciones, 'v20241214')"
echo ""
echo "   # Para análisis:"
echo "   performance <- obtener_performance_modelo()"
echo "   reporte <- generar_reporte_performance()"
echo ""
echo "📊 CONSULTAS ÚTILES:"
echo "   # Ver últimas predicciones:"
echo "   SELECT * FROM vista_monitoreo_predicciones LIMIT 10;"
echo ""
echo "   # Performance por contaminante:"
echo "   SELECT * FROM vista_performance_modelo WHERE nombre_magnitud = 'NO2';"
echo ""
echo "   # Actualizar valores reales:"
echo "   SELECT actualizar_valores_reales();"

echo ""
echo "💡 PRÓXIMOS PASOS:"
echo "   1. Ejecutar primera predicción con logging"
echo "   2. Configurar GitHub Actions con logging habilitado"
echo "   3. Programar actualización automática de valores reales"
echo "   4. Crear dashboard de monitoreo de performance"