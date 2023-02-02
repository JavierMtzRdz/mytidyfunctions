#' Tema para mapas
#'
#' Esta resume las funciones de estilo utilizadas en mis gráficas
#' con estilo html
#'
#' @param ... cambios en el tema
#'
#' @return Tema en html para gráficas de ggplot
#' @export

theme_maps_jmr <- function(...) {

  ggplot2::theme_void() +
    theme(legend.position = "top",
          # legend.position = c(.75, .825),
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
          legend.margin = margin(0,0,0,0),
          legend.spacing = unit(0, "cm"),
          panel.spacing = unit(c(0, 0, 0, 0), "null")) +
    theme(...)

}


#' Fijar crs de IMS
#'
#' ...
#'
#' @param .x
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
#' @param .x
#' 
#'
#' @return sf object
#' @export

add_zoom <- function(coor_xmin = -100.29,
                     coor_xmax = -93.71,
                     coor_ymin = 15.6,
                     coor_ymax = 20.21,
                     ub_xmin = -119.5,
                     ub_xmax = -106,
                     ub_ymin = 13.5,
                     ub_ymax = 22.8) {

  structure(
    "Make zoom",
    class = "my_zoom",
    fn = "add_zoom_fun",
    coor_xmin = coor_xmin,
    coor_xmax = coor_xmax,
    coor_ymin = coor_ymin,
    coor_ymax = coor_ymax,
    ub_xmin = ub_xmin,
    ub_xmax = ub_xmax,
    ub_ymin = ub_ymin,
    ub_ymax = ub_ymax)
}

#' Añadir método para hacer el zoom
#'
#' ...
#'
#' @param ..x
#' 
#' @importFrom ggplot2 ggplot_add
#'
#' @return sf object
#' @export
ggplot_add.my_zoom <- function(object, plot, object_name) {

  fn <- attr(object, "fn")

  args <- attributes(object)[!names(attributes(object)) %in%
                               c("class", "fn")]


  new_plot <- do.call(
    fn,
    c(list(plot), args)
  )


  return(new_plot)
}

#' Función interna para hacer zoom
#'
#' ...
#'
#' @param .plot plots
#'
#' @return sf object
#' @export

add_zoom_fun <- function(.plot,
                         coor_xmin = -100.29,
                         coor_xmax = -93.71,
                         coor_ymin = 15.6,
                         coor_ymax = 20.21,
                         ub_xmin = -119.5,
                         ub_xmax = -106,
                         ub_ymin = 13.5,
                         ub_ymax = 22.8) {

  num_layers <- length(.plot$layers)

  type <- sapply(1:num_layers,
                 function(y) {
                   class(.plot$layers[[y]]$geom)[1]
                 })

  if(any(type %in% c("GeomTextRepel",
                     "GeomText"))) {

    min_position <- min(c(1:num_layers)[type %in% c("GeomTextRepel",
                                                    "GeomText")]) - 1

  } else {
    min_position <- num_layers
  }


  plot1 <- .plot
  plot1$layers <- append(.plot$layers,
                         list(ggplot2::geom_rect(xmin = coor_xmin,
                                                 xmax = coor_xmax,
                                                 ymin = coor_ymin,
                                                 ymax = coor_ymax,
                                                 size = 0.35,
                                                 fill = "transparent",
                                                 color = "grey10")),
                         after = min_position)
  # plot1
  #
  # !length(.plot$layers)

  # plot1 <- insertLayer(.plot,
  #                      after = min_position,
  #                      ggplot2::geom_rect(xmin = -100.29,
  #                                         xmax = -93.71,
  #                                         ymin = 15.6,
  #                                         ymax = 20.21,
  #                                         fill = "transparent",
  #                                         color = "grey10"))

  zoom_plot <- .plot

  zoom_plot <- .plot +
    ggplot2::labs(title = ggplot2::element_blank(),
                  y = ggplot2::element_blank(),
                  x = ggplot2::element_blank(),
                  color = ggplot2::element_blank(),
                  fill = ggplot2::element_blank(),
                  linetype = ggplot2::element_blank(),
                  caption = ggplot2::element_blank()) +
    ggplot2::guides(fill = "none") +
    ggplot2::coord_sf(ylim = c(coor_ymin, coor_ymax),
                      xlim = c(coor_xmin, coor_xmax)) +
    ggplot2::ylim(coor_ymin, coor_ymax) +
    ggplot2::xlim(coor_xmin, coor_xmax) +
    ggplot2::theme(panel.background = ggplot2::element_rect(colour = "black",
                                          fill = "transparent",
                                          linewidth = 1))



  plot_out <- plot1 -
    ggplot2::annotation_custom(
      ggplot2::ggplotGrob(zoom_plot),
      xmin = ub_xmin, xmax = ub_xmax,
      ymin = ub_ymin, ymax = ub_ymax)



  return(plot_out)

}


#' Límites determinados por geometries
#'
#' ...
#'
#' @param .x
#'
#' @return layer con límites
#' @export
make_limit <- function(.x,
                       lon_buff = 0.2,
                       lat_buff = 0.2){

  area <- .x %>%
    sf::st_coordinates() %>%
    data.frame() %>%
    dplyr::transmute(x_lon = X,
              y_lat = Y)

  limits <- list(ggplot2::ylim(c(min(area$y_lat) - lat_buff,
                        max(area$y_lat) + lat_buff)),
                 ggplot2::xlim(c(min(area$x_lon) - lon_buff,
                        max(area$x_lon) + lon_buff)))


  return(limits)

}

#' Buffer to osm matrix
#'
#' ...
#'
#' @param .x
#'
#' @return layer con límites
#' @export

buffer_osm <- function(., buffer_x = 1, buffer_y = 1) {
  a <- .
  a[1] <- a[1]-buffer_x
  a[3] <- a[3]+buffer_x

  a[2] <- a[2]-buffer_y
  a[4] <- a[4]+buffer_y

  return(a)

}

#' Centroid from name
#'
#' ...
#'
#' @param .x
#'
#' @return layer con límites
#' @export
get_cent_oms <- function(lugar = NA) {

  lugar_matrix <- osmdata::getbb(lugar)

  df_cent <- dplyr::tibble(nombre = lugar,
                           lat = mean(lugar_matrix[2,]),
                           lon = mean(lugar_matrix[1,]))

  return(df_cent)
}


