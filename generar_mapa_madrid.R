#!/usr/bin/env Rscript
# Generar mapa estático de Madrid para la página principal

library(ggplot2)
library(dplyr)
library(sf)

# Cargar configuración global
source("app/global.R")

cat("=== GENERADOR DE MAPA MADRID ===\n")

# Cargar datos
if(file.exists("output/predicciones_40h_latest.rds")) {
  datos <- readRDS("output/predicciones_40h_latest.rds")
  cat("✅ Datos cargados:", nrow(datos), "filas\n")
} else {
  stop("❌ No se encuentra el archivo de predicciones")
}

# Extraer coordenadas si es sf
if("sf" %in% class(datos)) {
  coords <- st_coordinates(datos)
  datos$lon <- coords[,1]
  datos$lat <- coords[,2]
  datos <- st_drop_geometry(datos)
}

# Tomar primera hora de cada contaminante para el mapa
datos_mapa <- datos %>%
  group_by(contaminante, id_estacion) %>%
  slice(1) %>%
  ungroup()

cat("📍 Estaciones únicas:", length(unique(datos_mapa$id_estacion)), "\n")
cat("🏭 Contaminantes:", paste(unique(datos_mapa$contaminante), collapse = ", "), "\n")

# Función para generar mapa por contaminante
generar_mapa_contaminante <- function(datos_cont, contaminante_name) {
  
  # Añadir clasificación
  datos_cont$clasificacion <- sapply(datos_cont$prediccion, function(x) {
    clasificar_calidad_aire(contaminante_name, x)
  })
  
  # Crear mapa
  p <- ggplot(datos_cont, aes(x = lon, y = lat)) +
    # Fondo de Madrid
    geom_spatraster_rgb(data = madrid_mask) +
    # Añadir estaciones
    geom_point(aes(size = prediccion, color = clasificacion), 
              alpha = 0.8, stroke = 1.2) +
    scale_color_manual(
      values = COLORES_CALIDAD,
      name = "Calidad OMS",
      labels = c("bueno" = "Bueno", "moderado" = "Moderado", 
                "malo" = "Malo", "muy_malo" = "Muy Malo", "unknown" = "Desconocido")
    ) +
    scale_size_continuous(
      name = "µg/m³",
      range = c(1.5, 10.5),
      guide = guide_legend(override.aes = list(alpha = 1))
    ) +
    # Añadir nombres de estaciones principales
    geom_text_repel(
      aes(label = ifelse(prediccion > quantile(prediccion, 0.8, na.rm = TRUE) | 
                        prediccion < quantile(prediccion, 0.2, na.rm = TRUE),
                        paste0(substr(nombre_estacion, 1, 10), "\n", round(prediccion, 1)), "")),
      size = 2.8, max.overlaps = 8, force = 2
    ) +
    labs(
      title = paste("Mapa Calidad del Aire Madrid -", contaminante_name),
      subtitle = "Niveles actuales en estaciones de monitoreo",
      caption = "Colores basados en estándares OMS 2021",
      x = "Longitud", y = "Latitud"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      plot.caption = element_text(size = 9, color = "grey60"),
      legend.position = "bottom",
      legend.box = "horizontal"
    ) 
  
  return(p)
}

# Generar mapas para cada contaminante
contaminantes <- unique(datos_mapa$contaminante)
dir.create("app/www/mapas", showWarnings = FALSE, recursive = TRUE)

for(contaminante in contaminantes) {
  cat("\n🗺️ Generando mapa para:", contaminante, "\n")
  
  datos_cont <- datos_mapa %>% filter(contaminante == !!contaminante)
  
  if(nrow(datos_cont) > 0) {
    mapa <- generar_mapa_contaminante(datos_cont, contaminante)
    
    # Mapear nombre para archivo
    cont_archivo <- switch(contaminante,
      "Dióxido de Nitrógeno" = "no2",
      "Partículas < 10 µm" = "pm10", 
      "Ozono" = "o3"
    )
    
    archivo_salida <- paste0("app/www/mapas/mapa_", cont_archivo, ".png")
    
    ggsave(archivo_salida, plot = mapa, 
           width = 10, height = 8, dpi = 120, 
           bg = "white")
    
    cat("📄 Mapa guardado:", archivo_salida, "\n")
  }
}

cat("\n=== RESUMEN ===\n")
cat("✅ Mapas generados:", length(contaminantes), "\n")
cat("📁 Directorio:", "app/www/mapas/", "\n")
cat("🗺️ ¡Mapas listos para usar en el dashboard!\n")