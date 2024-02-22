
#' Tema inicial con formato propio
#'
#' Esta resume las funciones de estilo utilizadas en mis gráficas
#'
#' @param ... cambios en el tema
#'
#' @return Tema para gráficas de ggplot
#' @export
#'
theme_jmr <- function(...,
                      markdown = F) {
  ggplot2::theme_minimal() +
    ggplot2::theme(text = ggplot2::element_text(family = "Lato"),
                   axis.line = ggplot2::element_line(size = 0.3),
                   plot.title = formt_text(markdown = markdown,
                                           hjust = 0.5,
                                           size = 14, face = "bold",
                                           color = "grey20"),
                   plot.title.position = "plot",
                   plot.subtitle = formt_text(markdown = markdown,
                                              hjust = 0.5,
                                              size = 12,
                                              color = "gray50"),
                   plot.caption =  formt_text(markdown = markdown,
                                              color = "gray50",
                                              size = 10,
                                              hjust = 0),
                   panel.grid = ggplot2::element_line(linetype = 2,
                                                      size = 0.3,
                                                      color = "gray90"),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.key.width= unit(0.7, 'cm'),
                   legend.margin = margin(0,0,0,0),
                   legend.spacing = unit(0.1, "cm"),
                   legend.position = "top",
                   strip.background = ggplot2::element_rect(fill = "gray95",
                                                            linetype = "blank"),
                   panel.border = ggplot2::element_rect(color = "gray95",
                                                        fill = NA),
                   rect = ggplot2::element_rect(fill = "transparent")) +
    ggplot2::theme(...)
}


#' Paletas de colores de MCCI
#'
#' Es una lista de las paletas de MCCI
#'
#' @format list
#' @source Creación de comunicación de MCCI
paletas_mcci <- list(
  `general`  = c("#070B2D", "#F72732", "#7CDBA7", "#0E9A9D"),
  `bivariado1`  = c("#070B2D", "#3A425F", "#667598",
                    "#F72732", "#FF5454", "#FF9F9F"),
  `bivariado2`  = c("#070B2D", "#3A425F", "#667598", "#9FB0D4",
                    "#FF9F9F", "#FF5454", "#F72732", "#3C0605"),
  `variado`  = c("#070B2D", "#3A425F", "#667598", "#9FB0D4",
                 "#FF9F9F", "#FF5454", "#F72732", "#3C0605"),
  `multiple`  = c("#070B2D",
                  "#ff5454",
                  "#7cdba7",
                  "#0E9A9D",
                  "#F27F44",
                  "#f5e278",
                  "#2d6177")
)


#' Color interpolacion de paletas
#'
#' Esta función genera nuevos colores cuando es necesario y determina orde.
#'
#' @param palette Determina qué paleta tomar.
#' @param reverse Determina el orden de la paleta.
#' @param ... Demás elementos correspondientes a grDevices::colorRampPalette()
#'
#' @return regresa una función con un argumento integer (el número de colores)
#' y un vector de colores.
#' @export
mcci_pal <- function(palette = "general", reverse = FALSE, ...) {
  pal <- paletas_mcci[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  grDevices::colorRampPalette(pal, ...)
  
}

#' Color interpolacion de paletas manual
#'
#' Esta función genera nuevos colores cuando es necesario y determina orden.
#'
#' @param n número de colores.
#' @param palette Determina qué paleta tomar.
#' @param reverse Determina el orden de la paleta.
#' @param ... Demás elementos correspondientes a grDevices::colorRampPalette()
#'
#' @return regresa una función con un argumento integer (el número de colores)
#' y un vector de colores.
#' @export
mcci_pal_manual <- function(n, palette = "general", reverse = FALSE, ...) {
  pal <- paletas_mcci[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  grDevices::colorRampPalette(pal, ...)(n)
  
}

#' Función para añadir colores de MCCI a gráfica
#'
#' Esta función añade la paleta de colores a la gráfica de ggplot.
#'
#' @param palette Determina qué paleta tomar. Puede ser general, bivariado1, 
#' bivariado2, variado y multiple.
#' @param discrete Determina si los datos son discretos o continuos.
#' @param reverse Determina el orden de la paleta.
#' @param ... Demás elementos correspondientes a scale_color_gradientn()
#'
#' @return regresa una función con un argumento integer (el número de colores)
#' y un vector de colores.
#' @export
scale_color_mcci <- function(palette = "general", discrete = TRUE, reverse = FALSE, ...) {
  pal <- mcci_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    ggplot2::discrete_scale("colour", palette, palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}


#' Función para añadir fill MCCI a gráfica
#'
#' Esta función añade la paleta de fill a la gráfica de ggplot.
#'
#' @param palette Determina qué paleta tomar. Puede ser general, bivariado1, 
#' bivariado2, variado y multiple.
#' @param discrete Determina si los datos son discretos o continuos.
#' @param reverse Determina el orden de la paleta.
#' @param ... Demás elementos correspondientes a scale_color_gradientn()
#'
#' @return regresa una función con un argumento integer (el número de colores)
#' y un vector de colores.
#' @export
scale_fill_mcci <- function(palette = "general", discrete = TRUE, reverse = FALSE, ...) {
  pal <- mcci_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    ggplot2::discrete_scale("fill", palette, palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}

