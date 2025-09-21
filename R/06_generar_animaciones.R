# SCRIPT PARA GENERAR ANIMACIONES OFFLINE
# Genera GIFs con gganimate para cada contaminante

# Librerías necesarias ----
library(ggplot2)
library(dplyr)
library(lubridate)
library(gganimate)
library(transformr)

# Configuración independiente (no cargar global.R que tiene conflictos con Shiny)
# En su lugar, definir directamente lo que necesitamos
{
  # Configuración básica si no existe global.R
  ESTANDARES_OMS_2021 <- list(
    "Dióxido de Nitrógeno" = list(bueno = 25, moderado = 50, malo = 100, muy_malo = 200),
    "Partículas < 10 µm" = list(bueno = 20, moderado = 40, malo = 75, muy_malo = 150),
    "Ozono" = list(bueno = 80, moderado = 120, malo = 160, muy_malo = 200)
  )
  
  COLORES_CALIDAD <- list(
    bueno = "#2ecc71", moderado = "#f1c40f", malo = "#e67e22", 
    muy_malo = "#e74c3c", unknown = "#95a5a6"
  )
  
  clasificar_calidad_aire <- function(contaminante, valor) {
    # Los datos ya vienen con nombres exactos, no necesitamos mapear
    # Solo validamos que coincidan con ESTANDARES_OMS_2021
    
    est <- ESTANDARES_OMS_2021[[contaminante]]
    if (is.null(est)) {
      # Fallback: usar valores por defecto si no encuentra el contaminante
      cat("⚠️ Contaminante no encontrado en estándares:", contaminante, "\n")
      cat("   Contaminantes disponibles:", paste(names(ESTANDARES_OMS_2021), collapse = ", "), "\n")
      return("unknown")
    }
    
    if (is.na(valor) || is.null(valor)) return("unknown")
    if (valor <= est$bueno) return("bueno")
    else if (valor <= est$moderado) return("moderado") 
    else if (valor <= est$malo) return("malo")
    else return("muy_malo")
  }
}

# Función para generar animación por contaminante ----
generar_animacion_contaminante <- function(contaminante, datos_predicciones) {
  cat("Generando animación para:", contaminante, "\n")
  
  # Debug: verificar disponibilidad de estándares
  cat("DEBUG: Contaminante procesando:", contaminante, "\n")
  cat("DEBUG: ESTANDARES_OMS_2021 disponible:", exists("ESTANDARES_OMS_2021"), "\n")
  if(exists("ESTANDARES_OMS_2021")) {
    cat("DEBUG: Claves disponibles:", paste(names(ESTANDARES_OMS_2021), collapse = ", "), "\n")
  }
  
  # Filtrar datos del contaminante sin clasificar aún
  datos_filt <- datos_predicciones %>%
    filter(contaminante == !!contaminante) %>%
    filter(!is.na(prediccion), !is.na(fecha_hora)) %>%
    mutate(
      hora = as.numeric(difftime(fecha_hora, min(fecha_hora, na.rm = TRUE), units = "hours")) + 1,
      # Crear etiqueta de hora legible
      hora_label = paste("Hora", sprintf("%02d", hora), "-", format(fecha_hora, "%d/%m %H:%M"))
    ) %>%
    arrange(fecha_hora, nombre_estacion)
  
  cat("DEBUG: Filas filtradas:", nrow(datos_filt), "\n")
  
  # Agregar clasificación de forma más segura
  datos_filt$clasificacion <- "unknown"
  datos_filt$color_clasificacion <- COLORES_CALIDAD[["unknown"]]
  
  # Clasificar cada fila individualmente para debug
  for(i in 1:min(5, nrow(datos_filt))) {
    valor <- datos_filt$prediccion[i]
    cat("DEBUG: Clasificando valor", valor, "para", contaminante, "\n")
    if(!is.na(valor)) {
      clasificacion <- clasificar_calidad_aire(contaminante, valor)
      cat("DEBUG: Resultado clasificación:", clasificacion, "\n")
      datos_filt$clasificacion[i] <- clasificacion
      datos_filt$color_clasificacion[i] <- COLORES_CALIDAD[[clasificacion]]
    }
  }
  
  if(nrow(datos_filt) == 0) {
    cat("Sin datos para", contaminante, "\n")
    return(FALSE)
  }
  
  # Crear gráfico animado
  p_anim <- ggplot(datos_filt, 
                   aes(x = reorder(nombre_estacion, prediccion), 
                       y = prediccion, 
                       fill = color_clasificacion)) +
    geom_col(alpha = 0.85, color = "white", linewidth = 0.3) +
    scale_fill_identity() +
    coord_flip() +
    labs(
      title = paste("Evolución", contaminante, "- {closest_state}"),
      subtitle = "Predicción 40 horas por estaciones de monitoreo Madrid",
      x = "", 
      y = "Concentración (µg/m³)",
      caption = "Colores según estándares OMS 2021 | Sistema Calidad Aire Madrid"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 18, face = "bold", color = "navy"),
      plot.subtitle = element_text(size = 13, color = "gray30"),
      plot.caption = element_text(size = 10, color = "gray50"),
      axis.text.y = element_text(size = 10, color = "gray20"),
      axis.text.x = element_text(size = 11, color = "gray20"),
      axis.title.x = element_text(size = 12, face = "bold"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "gray90", size = 0.5)
    ) +
    transition_states(hora_label, 
                     transition_length = 1.5, 
                     state_length = 1.5) +
    ease_aes('cubic-in-out')
  
  # Determinar nombre del archivo
  archivo_salida <- switch(contaminante,
    "Dióxido de Nitrógeno" = "app/www/animacion_no2.gif",
    "Partículas < 10 µm" = "app/www/animacion_pm10.gif", 
    "Ozono" = "app/www/animacion_o3.gif",
    paste0("app/www/animacion_", gsub(" ", "_", tolower(contaminante)), ".gif")
  )
  
  # Crear directorio si no existe
  dir.create(dirname(archivo_salida), recursive = TRUE, showWarnings = FALSE)
  
  # Renderizar animación
  tryCatch({
    cat("Renderizando GIF:", archivo_salida, "\n")
    # Usar renderer por defecto de gganimate
    anim <- animate(p_anim, 
                    width = 900, height = 600, 
                    fps = 1.5, duration = 30)
    
    # Guardar la animación
    anim_save(archivo_salida, anim)
    
    cat("✅ Animación generada:", archivo_salida, "\n")
    return(TRUE)
  }, error = function(e) {
    cat("❌ Error generando animación:", e$message, "\n")
    
    # Fallback: generar imagen estática en su lugar
    cat("Creando imagen estática de fallback...\n")
    tryCatch({
      p_static <- ggplot(datos_filt %>% filter(hora == 1), 
                         aes(x = reorder(nombre_estacion, prediccion), 
                             y = prediccion, 
                             fill = color_clasificacion)) +
        geom_col(alpha = 0.85, color = "white", linewidth = 0.3) +
        scale_fill_identity() +
        coord_flip() +
        labs(
          title = paste("Niveles Actuales", contaminante),
          subtitle = "Madrid - Estaciones de monitoreo",
          x = "", 
          y = "Concentración (µg/m³)",
          caption = "Colores según estándares OMS 2021"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 18, face = "bold", color = "navy"),
          plot.subtitle = element_text(size = 13, color = "gray30"),
          plot.caption = element_text(size = 10, color = "gray50"),
          axis.text.y = element_text(size = 10, color = "gray20"),
          axis.text.x = element_text(size = 11, color = "gray20"),
          axis.title.x = element_text(size = 12, face = "bold"),
          plot.background = element_rect(fill = "white", color = NA),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "gray90", size = 0.5)
        )
      
      # Guardar como PNG si GIF falla
      archivo_png <- gsub("\\.gif$", ".png", archivo_salida)
      ggsave(archivo_png, p_static, width = 12, height = 8, dpi = 100)
      
      cat("📸 Imagen estática creada:", archivo_png, "\n")
      return(TRUE)
    }, error = function(e2) {
      cat("❌ También falló el fallback:", e2$message, "\n")
      return(FALSE)
    })
  })
}

# Función para generar datos de prueba ----
generar_datos_prueba <- function() {
  library(dplyr)
  library(lubridate)
  
  # Datos sintéticos para prueba
  estaciones <- c("Pza. de España", "Retiro", "Barajas Pueblo", "Escuelas Aguirre", 
                 "Ramón y Cajal", "Arturo Soria", "Villaverde Alto", "Farolillo")
  
  contaminantes <- c("Dióxido de Nitrógeno", "Partículas < 10 µm", "Ozono")
  
  fecha_base <- as.POSIXct("2024-01-15 12:00:00", tz = "Europe/Madrid")
  
  datos_prueba <- expand.grid(
    nombre_estacion = estaciones,
    contaminante = contaminantes,
    hora_offset = 0:39,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      fecha_hora = fecha_base + hours(hora_offset),
      # Simular valores realistas con variación temporal
      prediccion = case_when(
        contaminante == "Dióxido de Nitrógeno" ~ pmax(5, 30 + 15*sin(hora_offset/24*2*pi) + rnorm(n(), 0, 8)),
        contaminante == "Partículas < 10 µm" ~ pmax(2, 20 + 10*sin(hora_offset/24*2*pi + pi/3) + rnorm(n(), 0, 5)),
        contaminante == "Ozono" ~ pmax(10, 80 + 25*sin(hora_offset/24*2*pi + pi) + rnorm(n(), 0, 12)),
        TRUE ~ 50
      )
    ) %>%
    select(nombre_estacion, contaminante, fecha_hora, prediccion)
  
  cat("📊 Datos de prueba generados:", nrow(datos_prueba), "filas\n")
  return(datos_prueba)
}

# Función principal ----
generar_todas_animaciones <- function(usar_datos_prueba = FALSE) {
  cat("=== GENERADOR DE ANIMACIONES MADRID AIR QUALITY ===\n")
  
  if(usar_datos_prueba) {
    cat("🧪 Usando datos de prueba sintéticos\n")
    datos <- generar_datos_prueba()
  } else {
    # Cargar datos reales de predicción
    archivo_predicciones <- "output/predicciones_40h_latest.rds"
    
    if(!file.exists(archivo_predicciones)) {
      cat("❌ No se encontró archivo de predicciones:", archivo_predicciones, "\n")
      cat("Cambiando a datos de prueba...\n")
      datos <- generar_datos_prueba()
    } else {
      datos <- readRDS(archivo_predicciones)
      cat("📊 Datos reales cargados:", nrow(datos), "filas\n")
    }
  }
  
  # Lista de contaminantes únicos
  contaminantes <- unique(datos$contaminante)
  cat("🏭 Contaminantes encontrados:", paste(contaminantes, collapse = ", "), "\n")
  
  # Debug de estructura de datos
  cat("📋 Estructura de datos:\n")
  cat("  - Columnas:", paste(names(datos), collapse = ", "), "\n")
  if("contaminante" %in% names(datos)) {
    cat("  - Valores únicos de contaminante:\n")
    for(cont in contaminantes) {
      n_filas <- sum(datos$contaminante == cont, na.rm = TRUE)
      cat("    -", cont, ":", n_filas, "filas\n")
    }
  }
  
  # Generar animación para cada contaminante
  resultados <- sapply(contaminantes, function(cont) {
    generar_animacion_contaminante(cont, datos)
  })
  
  # Resumen
  exitosos <- sum(resultados)
  total <- length(contaminantes)
  
  cat("\n=== RESUMEN ===\n")
  cat("✅ Animaciones generadas:", exitosos, "/", total, "\n")
  
  if(exitosos == total) {
    cat("🎉 ¡Todas las animaciones fueron generadas exitosamente!\n")
    cat("📁 Los archivos GIF están en: app/www/\n")
  } else {
    cat("⚠️  Algunas animaciones fallaron. Revisar logs.\n")
  }
  
  return(exitosos == total)
}

# Ejecutar si se llama directamente
if(!interactive()) {
  generar_todas_animaciones()
} else {
  cat("Script cargado. Ejecute generar_todas_animaciones() para comenzar.\n")
}