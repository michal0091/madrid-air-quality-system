# FICHERO DE FUNCIONES AUXILIARES (UTILITIES)
# --------------------------------------------------------------------
# Contiene funciones reutilizables que pueden ser llamadas desde
# diferentes scripts del proyecto.
# --------------------------------------------------------------------

#' Procesa un lote de datos crudos, los transforma y los carga en la BBDD
#'
#' @param datos_crudos Un data.table con los datos en formato ancho, leídos desde un CSV.
#' @param db_conn Una conexión DBI activa a la base de datos PostgreSQL.
#' @param dim_estaciones Un data.table con la tabla de dimensiones de las estaciones.
#'
#' @return NULL (la función escribe en la BBDD como efecto secundario).
procesar_y_cargar_lote <- function(datos_crudos, db_conn, dim_estaciones) {
  
  log_info("Iniciando procesamiento de {nrow(datos_crudos)} filas crudas...")
  
  # Lógica de transformación de ancho a largo
  datos_largos <- melt(datos_crudos, 
                       id.vars = c("PROVINCIA", "MUNICIPIO", "ESTACION", "MAGNITUD", "PUNTO_MUESTREO", "ANO", "MES", "DIA"),
                       measure.vars = patterns("^H", "^V"),
                       variable.name = "hora_indice",
                       value.name = c("valor", "valido"))
                       
  datos_largos <- datos_largos[valido == "V"]
  if (nrow(datos_largos) == 0) {
    log_warn("El lote no contenía datos con validación 'V'. Omitiendo.")
    return(invisible(NULL))
  }
  datos_largos[, hora := as.integer(hora_indice) - 1]
  datos_largos[, `:=`(
      valor = as.numeric(sub(",", ".", valor, fixed = TRUE)),
      fecha_hora = make_datetime(ANO, MES, DIA, hora, tz = "Europe/Madrid")
  )]
  
  setnames(datos_largos, "MAGNITUD", "id_magnitud")
  setnames(datos_largos, "ESTACION", "id_estacion")
  
  # Cruzar con las dimensiones para obtener la clave primaria de las estaciones
  datos_enriquecidos <- merge(datos_largos, 
                              dim_estaciones[, .(codigo_largo, id_estacion)], 
                              by = "id_estacion",
                              all.x = TRUE) # Usamos all.x=TRUE para no perder filas si una estación no se encuentra
  
  if(any(is.na(datos_enriquecidos$id_estacion))) {
      log_warn("Se encontraron {sum(is.na(datos_enriquecidos$id_estacion))} mediciones de estaciones no presentes en 'dim_estaciones'. Serán omitidas.")
      datos_enriquecidos <- na.omit(datos_enriquecidos, cols = "id_estacion")
  }
  
  # Crear la tabla final de hechos, con las columnas exactas para la BBDD
  fact_mediciones_dt <- datos_enriquecidos[, .(
    id_estacion,
    id_magnitud,
    fecha_hora,
    valor_medido = valor
  )]
  
  # Escribir la tabla de hechos en la base de datos
  dbWriteTable(db_conn, "fact_mediciones", fact_mediciones_dt, append = TRUE)
  log_success("Cargados {nrow(fact_mediciones_dt)} registros en 'fact_mediciones'.")
  
  return(invisible(NULL))
}