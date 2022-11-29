#' Desagrupar por fila
#'
#' Esta función sirve para desagrupar un dataframe agrupado por
#' fila (dplyr::rowwise()).
#'
#' @param data dataframe agrupado por fila
#'
#' @return dataframe sin agrupación
#' @export
unrowwise_df <- function(data) {

  class(data) <- c("tbl_df", "data.frame")

  return(data)

}

#' Guardar base de datos en nuevo objeto
#'
#' Esta función sirve para guardar un objeto  tal como está en cierto punto
#' de una secuencia de pipas.
#'
#' @param data cualquier objeto
#' @param nombre string que se le asigna como nombre al objeto. Si no se
#' asigna ninguno, se guardará como obj_guardado .
#'
#' @return el mismo objeto y guarda un nuevo objeto con el nombre de bd_guardada
#' @export
guardar_obj <- function(data, nombre = "obj_guardado") {

  assign(nombre,
         data,
         envir = .GlobalEnv)

  return(data)

}



#' Transformar mes a número
#'
#' Esta resume las funciones de estilo utilizadas en mis gráficas
#' con estilo html
#'
#' @param ... cambios en el tema
#'
#' @return Tema en html para gráficas de ggplot
#' @export
month_to_number <- function(mes) {
  as.numeric(
    format(
      as.Date(
        paste0("01/", substr(mes, 1, 3), "/01"),
        format = "%y/%b/%d"),
      "%m"))
}

#' Transformar número a mes
#'
#' Esta resume las funciones de estilo utilizadas en mis gráficas
#' con estilo html
#'
#' @param ... cambios en el tema
#'
#' @return Tema en html para gráficas de ggplot
#' @export
number_to_month <- function(mes) {
  format(as.Date(paste0("01/", mes, "/01")),
         "%B",
         locale = readr::locale("es"))
}


