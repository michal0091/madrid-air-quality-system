# GUÍA DE MIGRACIÓN A CLOUD DATABASE

## Paso 1: Crear Base de Datos Cloud

### Opción A: Neon (Recomendado)
1. Ir a https://neon.tech
2. Crear cuenta gratuita
3. Crear nuevo proyecto: "madrid-air-quality"
4. Copiar datos de conexión:
   ```
   Host: ep-xyz-123.us-east-1.aws.neon.tech
   Database: neondb
   Username: tu-username
   Password: tu-password
   Port: 5432
   ```

### Opción B: Supabase
1. Ir a https://supabase.com
2. Crear cuenta gratuita
3. Crear nuevo proyecto: "madrid-air-quality"
4. Ir a Settings > Database
5. Copiar datos de conexión PostgreSQL

## Paso 2: Ejecutar Backup en Raspberry Pi

```bash
# En tu Raspberry Pi
cd /home/pi/madrid-air-quality-system
**Note**: Legacy backup scripts moved to `legacy-scripts` branch. Use current GitHub Actions deployment instead.

# Descargar backup a tu PC
scp pi@tu-pi:/home/pi/backups/madrid_air_quality_backup_*.sql.gz .
```

## Paso 3: Configurar Script de Restauración

Editar `scripts/restore_to_cloud.sh`:
```bash
# Actualizar con tus datos de Neon/Supabase
CLOUD_HOST="tu-host.neon.tech"
CLOUD_USER="tu-username"
CLOUD_DB="neondb"
```

## Paso 4: Restaurar a Cloud

```bash
# Configurar password
export PGPASSWORD='tu-password-cloud'

# Ejecutar restauración
**Note**: Legacy restore scripts moved to `legacy-scripts` branch. Current system uses GitHub Actions for automated deployment.
```

## Paso 5: Actualizar Configuración

### Archivo .Renviron (local)
```env
DB_HOST="tu-host.neon.tech"
DB_PORT="5432"
DB_NAME="neondb"
DB_USER="tu-username"
DB_PASSWORD="tu-password"
```

### GitHub Secrets (para Actions)
```
DB_HOST = tu-host.neon.tech
DB_PORT = 5432
DB_NAME = neondb
DB_USER = tu-username
DB_PASSWORD = tu-password
```

## Paso 6: Verificar Funcionamiento

```r
# Probar conexión local
source("R/utils.R")
con <- conectar_bd()
DBI::dbListTables(con)
DBI::dbDisconnect(con)
```

## Ventajas de la Migración

- ✅ **Seguridad**: Sin exposición de red local
- ✅ **Disponibilidad**: 99.9% uptime garantizado
- ✅ **Escalabilidad**: Crece automáticamente
- ✅ **Backup**: Automático y versionado
- ✅ **GitHub Actions**: Acceso directo sin VPN
- ✅ **Global**: Acceso desde cualquier lugar

## Costos

- **Neon Free**: 512MB, suficiente para el proyecto
- **Upgrade**: $19/mes para 10GB si creces
- **Comparación**: Más barato que electricidad Pi 24/7