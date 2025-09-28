# Generar madrid_mask offline para evitar problemas de memoria
# Este script debe ejecutarse antes de la generación de mapas

# Cargar librerías con manejo de errores
tryCatch({
  library(mapSpain)
  MAPSPAIN_AVAILABLE <- TRUE
}, error = function(e) {
  cat("❌ ERROR: mapSpain no disponible:", e$message, "\n")
  MAPSPAIN_AVAILABLE <- FALSE
})

library(sf)

tryCatch({
  library(tidyterra)
  TIDYTERRA_AVAILABLE <- TRUE
}, error = function(e) {
  cat("⚠️ tidyterra no disponible, continuando sin ella\n")
  TIDYTERRA_AVAILABLE <- FALSE
})

cat("Generando madrid_mask offline...\n")

# Crear directorio de salida si no existe
if (!dir.exists("data")) dir.create("data", recursive = TRUE)
if (!dir.exists("app")) dir.create("app", recursive = TRUE)

# Generar madrid_mask según disponibilidad de dependencias
if(MAPSPAIN_AVAILABLE) {
  cat("✅ Usando mapSpain para generar madrid_mask completo\n")

  tryCatch({
    # Generar datos espaciales de Madrid con mapSpain
    madrid <- esp_get_munic_siane(munic = "^Madrid$")
    madrid_b <- st_buffer(madrid, dist = 10)
    madrid_b <- st_as_sfc(st_bbox(madrid))

    cat("📥 Descargando tiles de Madrid...\n")
    madrid_mask <- esp_getTiles(madrid_b, type = "IGNBase.Todo", mask = TRUE, crop = TRUE, zoommin = 3)

    cat("✅ madrid_mask completo generado con mapSpain\n")

  }, error = function(e) {
    cat("❌ Error generando madrid_mask con mapSpain:", e$message, "\n")
    cat("🔄 Creando madrid_mask básico de fallback...\n")

    # Fallback: polígono básico de Madrid
    madrid_bbox <- st_bbox(c(xmin = -3.889091, ymin = 40.31443, xmax = -3.519518, ymax = 40.64290), crs = 4326)
    madrid_mask <- st_as_sfc(madrid_bbox)
    cat("⚠️ madrid_mask básico creado (sin tiles)\n")
  })

} else {
  cat("⚠️ mapSpain no disponible, creando madrid_mask básico\n")

  # Crear polígono básico de Madrid como fallback
  madrid_bbox <- st_bbox(c(xmin = -3.889091, ymin = 40.31443, xmax = -3.519518, ymax = 40.64290), crs = 4326)
  madrid_mask <- st_as_sfc(madrid_bbox)
  cat("✅ madrid_mask básico creado\n")
}

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