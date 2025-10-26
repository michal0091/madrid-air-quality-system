#!/usr/bin/env Rscript
# Generar imágenes PNG separadas por hora para animación JavaScript

library(ggplot2)
library(dplyr)
library(sf)
library(lubridate)

# Cargar configuración global
source("app/global.R")

cat("=== GENERADOR DE IMÁGENES POR HORA ===\n")

# Cargar datos (XGBoost nativo)
if(file.exists("output/predicciones_xgb_nativo_40h_latest.rds")) {
  datos <- readRDS("output/predicciones_xgb_nativo_40h_latest.rds")
  cat("✅ Datos cargados:", nrow(datos), "filas\n")
} else {
  stop("❌ No se encuentra el archivo de predicciones XGBoost")
}

# Extraer coordenadas si es sf
if("sf" %in% class(datos)) {
  coords <- st_coordinates(datos)
  datos$lon <- coords[,1]
  datos$lat <- coords[,2]
  datos <- st_drop_geometry(datos)
}

# Crear directorio para imágenes por hora
dir.create("app/www/horas", showWarnings = FALSE, recursive = TRUE)

# Obtener horas únicas
horas_unicas <- sort(unique(datos$fecha_hora))
# 5 contaminantes ICA oficiales
contaminantes <- c("Dióxido de Nitrógeno", "Partículas < 10 µm", "Partículas < 2.5 µm",
                   "Ozono", "Dióxido de Azufre")

cat("📅 Horas encontradas:", length(horas_unicas), "\n")
cat("🏭 Contaminantes ICA:", paste(contaminantes, collapse = ", "), "\n")

# Función para generar gráfico por hora y contaminante  
generar_grafico_hora <- function(datos_hora, contaminante, hora_actual) {
  
  # Añadir clasificación
  datos_hora$clasificacion <- sapply(datos_hora$prediccion, function(x) {
    clasificar_calidad_aire(contaminante, x)
  })
  
  # ORDEN FIJO: Ordenar por ID de estación para mantener consistencia
  datos_orden <- datos_hora %>% 
    arrange(id_estacion) %>%
    mutate(nombre_estacion = factor(nombre_estacion, levels = nombre_estacion))
  
  # Obtener estándares OMS para líneas de referencia
  estandares <- ESTANDARES_OMS_2021[[contaminante]]
  
  # Definir breaks personalizados según contaminante
  breaks_y <- switch(contaminante,
    "Dióxido de Nitrógeno" = seq(0, 200, by = 25),
    "Partículas < 10 µm" = seq(0, 150, by = 20),
    "Partículas < 2.5 µm" = seq(0, 100, by = 15),
    "Ozono" = seq(0, 200, by = 25),
    "Dióxido de Azufre" = seq(0, 150, by = 20),
    seq(0, 200, by = 25) # default
  )
  
  # Calcular límite superior del eje Y
  max_valor <- max(datos_orden$prediccion, na.rm = TRUE)
  y_limit <- max(max_valor * 1.1, max(breaks_y))
  
  p <- ggplot(datos_orden, aes(x = nombre_estacion, y = prediccion, fill = clasificacion)) +
    geom_col(alpha = 0.8, color = "white", linewidth = 0.3) +
    
    # Añadir líneas de referencia OMS (horizontales después de coord_flip)
    {
      if(!is.null(estandares)) {
        list(
          # Líneas con colores muy contrastantes y visibles
          geom_hline(yintercept = estandares$bueno, color = "black", 
                    linetype = "solid", linewidth = 1.5, alpha = 0.7),
          geom_hline(yintercept = estandares$moderado, color = "purple", 
                    linetype = "solid", linewidth = 1.5, alpha = 0.7),
          geom_hline(yintercept = estandares$malo, color = "navy", 
                    linetype = "solid", linewidth = 1.5, alpha = 0.7)
        )
      }
    } +
    
    scale_fill_manual(
      values = COLORES_CALIDAD,
      name = "Calidad OMS",
      labels = c("bueno" = "Bueno", "moderado" = "Moderado", 
                "malo" = "Malo", "muy_malo" = "Muy Malo")
    ) +
    scale_y_continuous(
      breaks = breaks_y,
      limits = c(0, y_limit),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      title = paste("Niveles Actuales", contaminante),
      subtitle = paste("Madrid - Estaciones ordenadas por ID -", format(hora_actual, "%d/%m/%Y %H:%M")),
      x = NULL, y = "Concentración (µg/m³)",
      caption = if(!is.null(estandares)) {
        paste("Límites OMS 2021: Negro =", estandares$bueno, "µg/m³ (Bueno) | Morado =", 
              estandares$moderado, "µg/m³ (Moderado) | Azul =", estandares$malo, "µg/m³ (Malo)")
      } else {
        "Estaciones ordenadas por ID"
      }
    ) +
    theme_minimal() +
    theme(
      # Títulos más grandes y legibles
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "grey20"),
      plot.subtitle = element_text(size = 14, hjust = 0.5, color = "grey30"),
      # Texto de ejes más grande
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "grey10"),
      axis.text.y = element_text(size = 12, face = "bold", color = "grey10"),
      axis.title.y = element_text(size = 14, face = "bold", color = "grey20"),
      # Leyenda más grande
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13, face = "bold"),
      # Grid simplificado
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.6),
      # Fondo blanco y caption más grande
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.caption = element_text(size = 10, hjust = 0.5, color = "grey20", face = "bold"),
      # Márgenes
      plot.margin = margin(15, 15, 15, 15)
    ) +
    coord_flip()
  
  return(p)
}

# Generar imágenes para cada contaminante y hora
total_imagenes <- 0

for(contaminante in contaminantes) {
  cat("\n🎯 Procesando:", contaminante, "\n")
  
  # Filtrar datos por contaminante
  datos_cont <- datos %>% filter(contaminante == !!contaminante)
  
  # Mapear nombre para archivo
  cont_archivo <- switch(contaminante,
    "Dióxido de Nitrógeno" = "no2",
    "Partículas < 10 µm" = "pm10",
    "Partículas < 2.5 µm" = "pm25",
    "Ozono" = "o3",
    "Dióxido de Azufre" = "so2"
  )
  
  # Procesar primeras 10 horas (para no crear demasiados archivos)
  # Seleccionar 10 horas estratégicas cada 4 horas para cubrir 40h totales (igual que mapas)
  indices_estrategicos <- seq(1, min(40, length(horas_unicas)), by = 4)[1:10]
  horas_procesar <- horas_unicas[indices_estrategicos[!is.na(indices_estrategicos)]]

  cat("📅 Horas estratégicas para gráficos:\n")
  for(i in seq_along(horas_procesar)) {
    cat(sprintf("  %02d: %s\n", i, format(horas_procesar[i], "%Y-%m-%d %H:%M")))
  }
  
  for(i in seq_along(horas_procesar)) {
    hora_actual <- horas_procesar[i]
    
    # Filtrar datos por hora
    datos_hora <- datos_cont %>% filter(fecha_hora == hora_actual)
    
    if(nrow(datos_hora) > 0) {
      # Generar gráfico
      p <- generar_grafico_hora(datos_hora, contaminante, hora_actual)
      
      # Guardar imagen
      archivo_salida <- sprintf("app/www/horas/%s_hora_%02d.png", cont_archivo, i)
      
      ggsave(archivo_salida, plot = p, 
             width = 12, height = 8, dpi = 100, 
             bg = "white")
      
      total_imagenes <- total_imagenes + 1
      cat("📸", sprintf("Hora %02d", i), "->", archivo_salida, "\n")
    }
  }
}

cat("\n=== RESUMEN ===\n")
cat("✅ Imágenes generadas:", total_imagenes, "\n")
cat("📁 Directorio:", "app/www/horas/", "\n")
cat("🎬 Listo para animación JavaScript\n")