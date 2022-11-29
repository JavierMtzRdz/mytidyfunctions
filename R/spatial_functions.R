#' Tema para mapas
#'
#' Esta resume las funciones de estilo utilizadas en mis gráficas
#' con estilo html
#'
#' @param ... cambios en el tema
#'
#' @return Tema en html para gráficas de ggplot
#' @export

theme_maps_jmr <- function(variables) {
  
  ggplot2::theme_void() +
    theme(legend.position = "top",
          legend.position = c(.75, .825),
          panel.background = ggplot2::element_rect(fill = "transparent",
                                                   color = "transparent"),
          plot.title.position = "plot",
          text = ggplot2::element_text(family = "Lato"),
          plot.title = ggplot2::element_text(hjust = 0.85,
                                             vjust = -10,
                                             size = 16, face = "bold", color = "grey20"),
          plot.subtitle = ggplot2::element_text(hjust = 0.5, 
                                                size = 12, color = "gray50"), 
          plot.caption = ggplot2::element_text(color = "gray50", 
                                               size = 10, hjust = 0.15),
          plot.margin = unit(c(0, 0, 0, 0), "null"),
          panel.border = ggplot2::element_blank(),
          panel.spacing = unit(c(0, 0, 0, 0), "null")) +
    theme()
  
}


#' Fijar crs de IMS
#'
#' ...
#'
#' @param ..x
#'
#' @return sf object
#' @export

c_pol_crs <- function(.x, crs = 4326) {
  # st_crs(.x$osm_polygons) <- st_crs(crs)
  
  for (i in names(.x[purrr::map_lgl(.x, function(x){(any(class(x) %in% "sf"))})])) {
    
    sf::st_crs(.x[[i]]) <-  sf::st_transform(crs)
    
  }
  
  return(.x)
}


#' Añadir zoom a mapa (centrado en el centro de México)
#'
#' ...
#'
#' @param ..x
#'
#' @return sf object
#' @export

add_zoom <- function(.plot = last_plot()) {
  insertLayer(.plot,
              after = 2,
              ggplot2::geom_rect(xmin = -100.29, 
                                 xmax = -93.71,   
                                 ymin = 15.6, 
                                 ymax = 20.21,  
                                 fill = "transparent",
                                 color = "grey10")) -
    ggplot2::annotation_custom(
      ggplot2::ggplotGrob(.plot +
                            ggplot2::geom_rect(xmin = -100.29, 
                                               xmax = -93.71,   
                                               ymin = 15.6, 
                                               ymax = 20.21,  
                                               fill = "transparent",
                                               color = "grey10") +
                            ggplot2::labs(title = ggplot2::element_blank(),
                                          y = ggplot2::element_blank(),
                                          x = ggplot2::element_blank(),
                                          color = ggplot2::element_blank(),
                                          fill = ggplot2::element_blank(),
                                          linetype = ggplot2::element_blank(),
                                          caption = ggplot2::element_blank()) +
                            ggplot2::guides(fill = "none") +
                            ggplot2::ylim(15.6, 20) +
                            ggplot2::xlim(-100, -94) +
                            ggplot2::coord_sf(ylim = c(15.6, 20),
                                              xlim = c(-100, -94))), 
      xmin = -119.5, xmax = -106, 
      ymin = 13.5, ymax = 22.8) 
  
}

