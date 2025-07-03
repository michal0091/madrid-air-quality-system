# FICHERO DE FUNCIONES AUXILIARES (UTILITIES)
# --------------------------------------------------------------------
# Contiene funciones reutilizables que pueden ser llamadas desde
# diferentes scripts del proyecto.
# --------------------------------------------------------------------

#' Procesa un lote de datos crudos de calidad del aire, los transforma y los carga en la BBDD
#'
#' Esta función toma datos de calidad del aire en formato ancho (con columnas H01-H24 para valores
#' y V01-V24 para validación), los transforma a formato largo, filtra por validez, 
#' enriquece con dimensiones de estaciones y carga los resultados en la tabla de hechos.
#'
#' @param datos_crudos Un data.table con los datos en formato ancho, leídos desde un CSV.
#'   Debe contener las columnas: PROVINCIA, MUNICIPIO, ESTACION, MAGNITUD, PUNTO_MUESTREO,
#'   ANO, MES, DIA, y columnas H01-H24 (valores) y V01-V24 (validación).
#' @param db_conn Una conexión DBI activa a la base de datos PostgreSQL donde se cargarán los datos.
#' @param dim_estaciones Un data.table con la tabla de dimensiones de las estaciones.
#'   Debe contener al menos las columnas: id_estacion, codigo_largo.
#'
#' @details
#' La función realiza las siguientes operaciones:
#' \itemize{
#'   \item Transforma los datos de formato ancho a largo usando data.table::melt
#'   \item Filtra solo las mediciones marcadas como válidas (V)
#'   \item Convierte los valores a numérico, manejando comas decimales
#'   \item Crea timestamps con zona horaria Europe/Madrid
#'   \item Cruza con dimensiones de estaciones para obtener claves primarias
#'   \item Carga los datos procesados en la tabla 'fact_mediciones'
#' }
#'
#' @return NULL (invisible). La función escribe en la BBDD como efecto secundario.
#'   En caso de error o datos vacíos, retorna NULL sin cargar datos.
#'
#' @examples
#' \dontrun{
#'   # Ejemplo de uso típico
#'   datos <- fread("datos_raw.csv")
#'   conn <- dbConnect(RPostgres::Postgres(), ...)
#'   dim_est <- dbReadTable(conn, "dim_estaciones")
#'   procesar_y_cargar_lote(datos, conn, dim_est)
#' }
#'
#' @seealso \code{\link[data.table]{melt}} para la transformación de formato,
#'   \code{\link[DBI]{dbWriteTable}} para la carga en base de datos
#'
#' @export
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