# UTILIDADES PARA EXPANSIÓN DE DATOS METEOROLÓGICOS DIARIOS A HORARIOS
# Implementa dos métodos: interpolación lineal simple y sinusoidal avanzada
# Incluye sistema de validación cruzada entre métodos

library(lubridate)
library(dplyr)
library(logger)

# ============================================================================
# MÉTODO 1: INTERPOLACIÓN LINEAL SIMPLE (VALIDACIÓN RÁPIDA)
# ============================================================================

#' Expansión lineal simple de datos meteorológicos diarios a horarios
#' 
#' @param datos_diarios data.frame con datos meteorológicos diarios
#' @return data.frame con datos horarios expandidos
expandir_meteo_lineal <- function(datos_diarios) {
  
  log_info("Iniciando expansión meteorológica lineal...")
  
  # Validar estructura de datos
  cols_requeridas <- c("fecha", "temp_media_c", "temp_maxima_c", "temp_minima_c",
                       "humedad_media_pct", "humedad_maxima_pct", "humedad_minima_pct",
                       "presion_maxima_hpa", "presion_minima_hpa", 
                       "vel_viento_media_ms", "precipitacion_mm")
  
  missing_cols <- setdiff(cols_requeridas, names(datos_diarios))
  if(length(missing_cols) > 0) {
    log_error("Columnas faltantes: {paste(missing_cols, collapse = ', ')}")
    stop("Datos incompletos")
  }
  
  # Función para interpolar linealmente entre máximo y mínimo
  interpolar_lineal_simple <- function(valor_min, valor_max, hora_min = 6, hora_max = 14) {
    # Crear vector de 24 horas (0-23)
    horas <- 0:23
    
    # Interpolación lineal simple: mínimo al amanecer, máximo mediodía
    valores_horarios <- ifelse(
      horas <= hora_max,
      valor_min + (valor_max - valor_min) * (horas - hora_min) / (hora_max - hora_min),
      valor_max - (valor_max - valor_min) * (horas - hora_max) / (24 - hora_max + hora_min)
    )
    
    # Ajustar valores fuera de rango
    valores_horarios[horas < hora_min] <- valor_min + 
      (valores_horarios[hora_min + 1] - valor_min) * (hora_min - horas[horas < hora_min]) / hora_min
    
    return(pmax(pmin(valores_horarios, valor_max), valor_min))
  }
  
  # Expandir cada día a 24 horas
  datos_expandidos <- datos_diarios %>%
    rowwise() %>%
    do({
      fecha_base <- .$fecha
      
      # Temperatura (min ~6am, max ~2pm)
      temp_horaria <- interpolar_lineal_simple(.$temp_minima_c, .$temp_maxima_c, 6, 14)
      
      # Humedad (pattern inverso: max ~6am, min ~2pm) 
      humedad_horaria <- interpolar_lineal_simple(.$humedad_minima_pct, .$humedad_maxima_pct, 14, 6)
      
      # Presión (cambio gradual, min ~5pm, max ~10am)
      presion_horaria <- interpolar_lineal_simple(.$presion_minima_hpa, .$presion_maxima_hpa, 17, 10)
      
      # Viento (constante con pequeña variabilidad)
      viento_horario <- rep(.$vel_viento_media_ms, 24) * runif(24, 0.8, 1.2)
      
      # Precipitación (distribuida aleatoriamente si hay)
      precip_horaria <- rep(0, 24)
      if(!is.na(.$precipitacion_mm) && .$precipitacion_mm > 0) {
        # Concentrar precipitación en 2-4 horas aleatorias
        horas_lluvia <- sample(0:23, size = min(4, max(2, round(.$precipitacion_mm/2))))
        precip_horaria[horas_lluvia + 1] <- .$precipitacion_mm / length(horas_lluvia)
      }
      
      # Crear data frame horario para este día
      data.frame(
        fecha_hora = as.POSIXct(paste(fecha_base, sprintf("%02d:00:00", 0:23)), 
                               format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Madrid"),
        fecha = fecha_base,
        hora = 0:23,
        temp_c = temp_horaria,
        humedad_pct = humedad_horaria,
        presion_hpa = presion_horaria,
        vel_viento_ms = viento_horario,
        precipitacion_mm = precip_horaria,
        metodo = "lineal"
      )
    }) %>%
    ungroup()
  
  log_info("Expansión lineal completada: {nrow(datos_expandidos)} registros horarios generados")
  return(datos_expandidos)
}

# ============================================================================
# MÉTODO 2: INTERPOLACIÓN SINUSOIDAL AVANZADA
# ============================================================================

#' Expansión sinusoidal avanzada usando horas exactas de máx/mín
#' 
#' @param datos_diarios data.frame con datos diarios incluyendo horas de extremos
#' @return data.frame con datos horarios expandidos
expandir_meteo_sinusoidal <- function(datos_diarios) {
  
  log_info("Iniciando expansión meteorológica sinusoidal...")
  
  # Función para interpolación sinusoidal con horas exactas
  interpolar_sinusoidal <- function(valor_medio, valor_min, valor_max, 
                                   hora_min = 6, hora_max = 14) {
    horas <- 0:23
    
    # Calcular amplitud y fase
    amplitud <- (valor_max - valor_min) / 2
    valor_base <- (valor_max + valor_min) / 2
    
    # Ajustar fase para que el máximo ocurra a hora_max
    fase <- hora_max - 6  # 6 corresponde a pi/2 en el ciclo de 24h
    
    # Función sinusoidal
    valores_horarios <- valor_base + amplitud * sin(2 * pi * (horas - fase) / 24)
    
    return(valores_horarios)
  }
  
  # Función para extraer hora de timestamp
  extraer_hora <- function(timestamp) {
    if(is.na(timestamp)) return(NA)
    return(as.numeric(format(timestamp, "%H")) + as.numeric(format(timestamp, "%M"))/60)
  }
  
  # Expandir cada día
  datos_expandidos <- datos_diarios %>%
    rowwise() %>%
    do({
      fecha_base <- .$fecha
      
      # Extraer horas exactas cuando están disponibles
      hora_temp_min <- ifelse(is.na(.$temp_minima_hora), 6, extraer_hora(.$temp_minima_hora))
      hora_temp_max <- ifelse(is.na(.$temp_maxima_hora), 14, extraer_hora(.$temp_maxima_hora))
      hora_hum_min <- ifelse(is.na(.$hora_humedad_minima), 14, extraer_hora(.$hora_humedad_minima))
      hora_hum_max <- ifelse(is.na(.$hora_humedad_maxima), 6, extraer_hora(.$hora_humedad_maxima))
      
      # Temperatura sinusoidal
      temp_horaria <- interpolar_sinusoidal(
        .$temp_media_c, .$temp_minima_c, .$temp_maxima_c,
        hora_temp_min, hora_temp_max
      )
      
      # Humedad sinusoidal (patrón inverso)
      humedad_horaria <- interpolar_sinusoidal(
        .$humedad_media_pct, .$humedad_minima_pct, .$humedad_maxima_pct,
        hora_hum_min, hora_hum_max
      )
      
      # Presión con spline suave
      if(!is.na(.$presion_minima_hpa) && !is.na(.$presion_maxima_hpa)) {
        hora_pres_min <- ifelse(is.na(.$hora_presion_minima), 17, extraer_hora(.$hora_presion_minima))
        hora_pres_max <- ifelse(is.na(.$hora_presion_maxima), 10, extraer_hora(.$hora_presion_maxima))
        
        presion_horaria <- interpolar_sinusoidal(
          (.$presion_minima_hpa + .$presion_maxima_hpa) / 2,
          .$presion_minima_hpa, .$presion_maxima_hpa,
          hora_pres_min, hora_pres_max
        )
      } else {
        # Fallback a valores medios si no hay datos
        presion_horaria <- rep(coalesce(.$presion_maxima_hpa, .$presion_minima_hpa, 1013), 24)
      }
      
      # Viento con variabilidad realista
      viento_base <- .$vel_viento_media_ms
      if(is.na(viento_base)) viento_base <- 2.0
      
      # Añadir ciclo diurno al viento (más fuerte durante el día)
      ciclo_viento <- 1 + 0.3 * sin(2 * pi * (0:23 - 12) / 24)
      viento_horario <- viento_base * ciclo_viento * runif(24, 0.7, 1.3)
      
      # Precipitación distribuida probabilísticamente
      precip_horaria <- rep(0, 24)
      if(!is.na(.$precipitacion_mm) && .$precipitacion_mm > 0) {
        # Distribución más realista: más probable en tarde/noche
        prob_horas <- c(rep(0.5, 6), rep(1.5, 6), rep(2.0, 6), rep(1.0, 6))  # 0-5, 6-11, 12-17, 18-23
        prob_horas <- prob_horas / sum(prob_horas)
        
        n_horas_lluvia <- max(1, min(8, rpois(1, lambda = .$precipitacion_mm/3 + 1)))
        horas_lluvia <- sample(0:23, size = n_horas_lluvia, prob = prob_horas)
        
        # Distribución gamma para intensidades
        intensidades <- rgamma(n_horas_lluvia, shape = 2, rate = 2/.$precipitacion_mm * n_horas_lluvia)
        intensidades <- intensidades * .$precipitacion_mm / sum(intensidades)  # Normalizar
        
        precip_horaria[horas_lluvia + 1] <- intensidades
      }
      
      # Crear data frame horario
      data.frame(
        fecha_hora = as.POSIXct(paste(fecha_base, sprintf("%02d:00:00", 0:23)), 
                               format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Madrid"),
        fecha = fecha_base,
        hora = 0:23,
        temp_c = temp_horaria,
        humedad_pct = humedad_horaria,
        presion_hpa = presion_horaria,
        vel_viento_ms = viento_horario,
        precipitacion_mm = precip_horaria,
        metodo = "sinusoidal"
      )
    }) %>%
    ungroup()
  
  log_info("Expansión sinusoidal completada: {nrow(datos_expandidos)} registros horarios")
  return(datos_expandidos)
}

# ============================================================================
# SISTEMA DE VALIDACIÓN CRUZADA
# ============================================================================

#' Comparar métodos de interpolación cuando hay datos reales disponibles
#' 
#' @param datos_diarios data.frame con datos diarios
#' @param datos_horarios_reales data.frame con datos horarios reales (para validación)
#' @return list con métricas de comparación
validar_metodos_interpolacion <- function(datos_diarios, datos_horarios_reales = NULL) {
  
  log_info("Iniciando validación cruzada de métodos de interpolación...")
  
  # Generar datos con ambos métodos
  datos_lineal <- expandir_meteo_lineal(datos_diarios)
  datos_sinusoidal <- expandir_meteo_sinusoidal(datos_diarios)
  
  resultados <- list(
    datos_lineal = datos_lineal,
    datos_sinusoidal = datos_sinusoidal,
    metricas = list()
  )
  
  # Si hay datos reales, calcular métricas de error
  if(!is.null(datos_horarios_reales)) {
    
    log_info("Calculando métricas de error contra datos reales...")
    
    # Preparar datos reales para comparación
    datos_reales_prep <- datos_horarios_reales %>%
      select(fecha_hora, temp_real = temp_c, humedad_real = humedad_pct, 
             presion_real = presion_hpa) %>%
      filter(!is.na(temp_real))
    
    # Función para calcular métricas
    calcular_metricas <- function(datos_pred, sufijo) {
      datos_comp <- datos_pred %>%
        inner_join(datos_reales_prep, by = "fecha_hora") %>%
        filter(!is.na(temp_c), !is.na(temp_real))
      
      if(nrow(datos_comp) == 0) {
        log_warn("No hay datos para comparación con método {sufijo}")
        return(NULL)
      }
      
      list(
        n_observaciones = nrow(datos_comp),
        temp_rmse = sqrt(mean((datos_comp$temp_c - datos_comp$temp_real)^2, na.rm = TRUE)),
        temp_mae = mean(abs(datos_comp$temp_c - datos_comp$temp_real), na.rm = TRUE),
        temp_r2 = cor(datos_comp$temp_c, datos_comp$temp_real, use = "complete.obs")^2,
        humedad_rmse = sqrt(mean((datos_comp$humedad_pct - datos_comp$humedad_real)^2, na.rm = TRUE)),
        humedad_mae = mean(abs(datos_comp$humedad_pct - datos_comp$humedad_real), na.rm = TRUE),
        humedad_r2 = cor(datos_comp$humedad_pct, datos_comp$humedad_real, use = "complete.obs")^2
      )
    }
    
    resultados$metricas$lineal <- calcular_metricas(datos_lineal, "lineal")
    resultados$metricas$sinusoidal <- calcular_metricas(datos_sinusoidal, "sinusoidal")
    
    # Determinar mejor método
    if(!is.null(resultados$metricas$lineal) && !is.null(resultados$metricas$sinusoidal)) {
      rmse_temp_lineal <- resultados$metricas$lineal$temp_rmse
      rmse_temp_sinusoidal <- resultados$metricas$sinusoidal$temp_rmse
      
      mejor_metodo <- ifelse(rmse_temp_lineal < rmse_temp_sinusoidal, "lineal", "sinusoidal")
      resultados$mejor_metodo <- mejor_metodo
      
      log_info("Mejor método basado en RMSE temperatura: {mejor_metodo}")
      log_info("RMSE Lineal: {round(rmse_temp_lineal, 3)}, Sinusoidal: {round(rmse_temp_sinusoidal, 3)}")
    }
  } else {
    log_info("Sin datos reales para validación - generando ambos métodos para comparación visual")
    resultados$mejor_metodo <- "sinusoidal"  # Default al más sofisticado
  }
  
  return(resultados)
}

#' Función helper para cargar datos meteorológicos desde BD
#' 
#' @param db_conn conexión a base de datos
#' @param fecha_inicio fecha de inicio (default: 30 días atrás)
#' @param fecha_fin fecha de fin (default: hoy)
#' @return data.frame con datos meteorológicos diarios
cargar_datos_meteo_bd <- function(db_conn, fecha_inicio = Sys.Date() - 30, fecha_fin = Sys.Date()) {
  
  query <- "
    SELECT 
      fecha,
      temp_media_c,
      temp_maxima_c, 
      temp_minima_c,
      temp_maxima_hora,
      temp_minima_hora,
      humedad_media_pct,
      humedad_maxima_pct,
      humedad_minima_pct,
      hora_humedad_maxima,
      hora_humedad_minima,
      presion_maxima_hpa,
      presion_minima_hpa,
      hora_presion_maxima,
      hora_presion_minima,
      vel_viento_media_ms,
      precipitacion_mm
    FROM fact_meteo_diaria 
    WHERE fecha BETWEEN $1 AND $2
      AND temp_media_c IS NOT NULL
    ORDER BY fecha
  "
  
  datos <- DBI::dbGetQuery(db_conn, query, params = list(fecha_inicio, fecha_fin))
  
  log_info("Cargados {nrow(datos)} días de datos meteorológicos desde BD")
  return(datos)
}