#!/bin/bash

# VERIFICAR ESTRUCTURA DE TABLAS
# Para identificar nombres correctos de columnas

# Configuración
DB_NAME="air_quality_db"
DB_USER="air_quality_user"

echo "🔍 ESTRUCTURA DE TABLAS"
echo "======================="

echo ""
echo "📊 COLUMNAS DE fact_mediciones:"
psql -h localhost -U $DB_USER -d $DB_NAME -c "\d fact_mediciones"

echo ""
echo "📊 COLUMNAS DE fact_meteo_diaria:"
psql -h localhost -U $DB_USER -d $DB_NAME -c "\d fact_meteo_diaria"

echo ""
echo "📊 MUESTRA DE DATOS fact_mediciones (primeros 5 registros):"
psql -h localhost -U $DB_USER -d $DB_NAME -c "SELECT * FROM fact_mediciones LIMIT 5;"

echo ""
echo "📊 RANGO DE FECHAS EN fact_mediciones:"
psql -h localhost -U $DB_USER -d $DB_NAME -c "
SELECT
    min(fecha_hora) as fecha_minima,
    max(fecha_hora) as fecha_maxima,
    count(*) as total_registros
FROM fact_mediciones;
"