#!/usr/bin/env Rscript
# Generar mapas PNG separados por hora para animación JavaScript

library(ggplot2)
library(dplyr)
library(sf)
library(lubridate)

# Cargar configuración global
source("app/global.R")

cat("=== GENERADOR DE MAPAS POR HORA ===\n")

# Cargar datos DE PREDICCIÓN (objetivo de la app)
if(file.exists("output/predicciones_40h_latest.rds")) {
  datos <- readRDS("output/predicciones_40h_latest.rds")
  cat("✅ Datos de PREDICCIÓN cargados:", nrow(datos), "filas\n")
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

# Crear directorio para mapas por hora
dir.create("app/www/mapas_horas", showWarnings = FALSE, recursive = TRUE)

# Obtener horas únicas
horas_unicas <- sort(unique(datos$fecha_hora))
contaminantes <- c("Dióxido de Nitrógeno", "Partículas < 10 µm", "Ozono")

cat("📅 Horas encontradas:", length(horas_unicas), "\n")
cat("🏭 Contaminantes:", paste(contaminantes, collapse = ", "), "\n")

# Función para generar mapa por hora y contaminante
generar_mapa_hora <- function(datos_hora, contaminante, hora_actual) {
  
  # Añadir clasificación
  datos_hora$clasificacion <- sapply(datos_hora$prediccion, function(x) {
    clasificar_calidad_aire(contaminante, x)
  })
  
  # Añadir categorización para tamaño manual de burbujas
  datos_hora$categoria_tamanio <- sapply(datos_hora$prediccion, function(x) {
    if(is.na(x)) return("Bajo")
    
    switch(contaminante,
      "Dióxido de Nitrógeno" = {
        if(x < 25) "Bajo"
        else if(x < 50) "Medio" 
        else if(x < 100) "Alto"
        else "Muy Alto"
      },
      "Partículas < 10 µm" = {
        if(x < 20) "Bajo"
        else if(x < 40) "Medio"
        else if(x < 75) "Alto" 
        else "Muy Alto"
      },
      "Ozono" = {
        if(x < 80) "Bajo"
        else if(x < 120) "Medio"
        else if(x < 160) "Alto"
        else "Muy Alto" 
      },
      # default NO2
      {
        if(x < 25) "Bajo"
        else if(x < 50) "Medio"
        else if(x < 100) "Alto" 
        else "Muy Alto"
      }
    )
  })
  
  # Convertir a factor con niveles ordenados
  datos_hora$categoria_tamanio <- factor(datos_hora$categoria_tamanio, 
                                        levels = c("Bajo", "Medio", "Alto", "Muy Alto"))
  
  # Asegurar que todos los niveles estén presentes añadiendo puntos invisibles
  niveles_completos <- c("Bajo", "Medio", "Alto", "Muy Alto")
  niveles_presentes <- levels(droplevels(datos_hora$categoria_tamanio))
  niveles_faltantes <- setdiff(niveles_completos, niveles_presentes)
  
  if(length(niveles_faltantes) > 0) {
    # Añadir puntos invisibles para niveles faltantes
    for(nivel in niveles_faltantes) {
      punto_invisible <- data.frame(
        lon = mean(datos_hora$lon, na.rm = TRUE),
        lat = mean(datos_hora$lat, na.rm = TRUE),
        prediccion = 0,
        clasificacion = "bueno",
        categoria_tamanio = factor(nivel, levels = niveles_completos),
        # Copiar otras columnas necesarias
        stringsAsFactors = FALSE
      )
      # Copiar otras columnas que puedan existir
      columnas_extra <- setdiff(names(datos_hora), names(punto_invisible))
      for(col in columnas_extra) {
        punto_invisible[[col]] <- datos_hora[[col]][1]
      }
      
      datos_hora <- rbind(datos_hora, punto_invisible)
    }
  }
  
  # Crear mapa con fondo de Madrid real
  p <- ggplot() +
    # Fondo de Madrid real
    geom_spatraster_rgb(data = madrid_mask) 

  # Separar datos reales de puntos invisibles
  datos_reales <- datos_hora[datos_hora$prediccion > 0, ]
  datos_invisibles <- datos_hora[datos_hora$prediccion == 0, ]
  
  p <- p + 
    # Añadir puntos invisibles para completar leyenda (alpha = 0)
    geom_point(data = datos_invisibles, 
               aes(x = lon, y = lat, size = categoria_tamanio, color = clasificacion), 
               alpha = 0) +
    # Añadir estaciones reales
    geom_point(data = datos_reales, 
               aes(x = lon, y = lat, size = categoria_tamanio, color = clasificacion), 
               alpha = 0.9, stroke = 1.5) +
    scale_color_manual(
      values = COLORES_CALIDAD,
      name = "Calidad OMS",
      labels = c("bueno" = "Bueno", "moderado" = "Moderado", 
                "malo" = "Malo", "muy_malo" = "Muy Malo", "unknown" = "Desconocido"),
      drop = FALSE
    ) +
    # Leyenda manual con tamaños fijos y consistentes
    scale_size_manual(
      name = "Concentración (µg/m³)",
      values = c("Bajo" = 4, "Medio" = 7, "Alto" = 10, "Muy Alto" = 13),
      breaks = c("Bajo", "Medio", "Alto", "Muy Alto"),
      limits = c("Bajo", "Medio", "Alto", "Muy Alto"),
      labels = switch(contaminante,
        "Dióxido de Nitrógeno" = c("< 25", "25-50", "50-100", "> 100"),
        "Partículas < 10 µm" = c("< 20", "20-40", "40-75", "> 75"), 
        "Ozono" = c("< 80", "80-120", "120-160", "> 160"),
        c("< 25", "25-50", "50-100", "> 100") # default
      ),
      drop = FALSE,
      guide = guide_legend(
        override.aes = list(alpha = 0.9, color = "grey50"),
        title.position = "top",
        title.hjust = 0.5,
        nrow = 1,
        keywidth = 1.2,
        keyheight = 1.2
      )
    ) +
    # Etiquetas para estaciones más significativas
    geom_text_repel(
      data = datos_reales,
      aes(x = lon, y = lat,
        label = ifelse(prediccion > quantile(prediccion, 0.85, na.rm = TRUE) |
          prediccion < quantile(prediccion, 0.15, na.rm = TRUE),
        paste0(substr(nombre_estacion, 1, 8), "\n", round(prediccion, 1)), "")),
      size = 3.5, max.overlaps = 6, force = 3, 
      color = "white", fontface = "bold",
      bg.color = "black", bg.r = 0.3, alpha = 0.8
    ) +
    labs(
      title = paste("Predicción Madrid -", contaminante),
      subtitle = paste("Fecha:", format(hora_actual, "%d/%m/%Y %H:%M"), "| Modelo CARET Random Forest R² > 92%"),
      caption = "Predicciones basadas en modelo ML | Colores: estándares OMS 2021"
    ) +
    scale_x_continuous(limits = c(-3.889091, -3.519518)) +
    scale_y_continuous(limits = c(40.31443, 40.64290)) +
    theme_void() +
    theme(
      # Títulos grandes y legibles
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5, color = "grey15"),
      plot.subtitle = element_text(size = 13, hjust = 0.5, color = "grey25"),
      plot.caption = element_text(size = 11, hjust = 0.5, color = "grey40"),
      # Leyenda optimizada
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12, face = "bold"),
      # Márgenes
      plot.margin = margin(10, 10, 10, 10),
      plot.background = element_rect(fill = "white", color = NA)
    )
  
  return(p)
}

# Generar mapas para cada contaminante y hora
total_mapas <- 0

for(contaminante in contaminantes) {
  cat("\n🗺️ Procesando mapas:", contaminante, "\n")
  
  # Filtrar datos por contaminante
  datos_cont <- datos %>% filter(contaminante == !!contaminante)
  
  # Mapear nombre para archivo
  cont_archivo <- switch(contaminante,
    "Dióxido de Nitrógeno" = "no2",
    "Partículas < 10 µm" = "pm10", 
    "Ozono" = "o3"
  )
  
  # Procesar primeras 10 horas (para coherencia con gráficos)
  horas_procesar <- head(horas_unicas, 10)
  
  for(i in seq_along(horas_procesar)) {
    hora_actual <- horas_procesar[i]
    
    # Filtrar datos por hora
    datos_hora <- datos_cont %>% filter(fecha_hora == hora_actual)
    
    if(nrow(datos_hora) > 0) {
      # Generar mapa
      p <- generar_mapa_hora(datos_hora, contaminante, hora_actual)
      
      # Guardar mapa
      archivo_salida <- sprintf("app/www/mapas_horas/mapa_%s_hora_%02d.png", cont_archivo, i)
      
      ggsave(archivo_salida, plot = p, 
             width = 14, height = 10, dpi = 120, 
             bg = "white")
      
      total_mapas <- total_mapas + 1
      cat("🗺️", sprintf("Hora %02d", i), "->", archivo_salida, "\n")
    }
  }
}

cat("\n=== RESUMEN MAPAS ===\n")
cat("✅ Mapas generados:", total_mapas, "\n")
cat("📁 Directorio:", "app/www/mapas_horas/", "\n")
cat("🎬 Listo para animación de mapas JavaScript\n")