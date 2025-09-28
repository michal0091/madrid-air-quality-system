# Generar madrid_mask offline para evitar problemas de memoria
# Este script debe ejecutarse antes de la generación de mapas

library(mapSpain)
library(sf)
library(tidyterra)

cat("Generando madrid_mask offline...\n")

# Crear directorio de salida si no existe
if (!dir.exists("data")) dir.create("data", recursive = TRUE)
if (!dir.exists("app")) dir.create("app", recursive = TRUE)

# Generar datos espaciales de Madrid
madrid <- esp_get_munic_siane(munic = "^Madrid$")
madrid_b <- st_buffer(madrid, dist = 10)
madrid_b <- st_as_sfc(st_bbox(madrid))

cat("Descargando tiles de Madrid...\n")
madrid_mask <- esp_getTiles(madrid_b, type = "IGNBase.Todo", mask = TRUE, crop = TRUE, zoommin = 3)

# Guardar en múltiples ubicaciones
saveRDS(madrid_mask, "data/madrid_mask.rds")
saveRDS(madrid_mask, "app/madrid_mask.rds")

cat("madrid_mask generado y guardado en:\n")
cat("- data/madrid_mask.rds\n")
cat("- app/madrid_mask.rds\n")

# Información del objeto
cat("\nInformación del objeto madrid_mask:\n")
cat("Clase:", class(madrid_mask), "\n")
cat("Dimensiones:", dim(madrid_mask), "\n")
cat("Tamaño del archivo:", round(file.size("data/madrid_mask.rds") / 1024 / 1024, 2), "MB\n")