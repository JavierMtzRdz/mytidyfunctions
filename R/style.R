
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
                      text_resize = 1, 
                      markdown = F) {
  ggplot2::theme_minimal() +
    ggplot2::theme(text = ggplot2::element_text(family = "Lato",
                                                size = 11*text_resize),
                   axis.line = ggplot2::element_line(size = 0.3),
                   plot.title = formt_text(markdown = markdown,
                                           hjust = 0.5,
                                           size = 14*text_resize, face = "bold",
                                           color = "grey20"),
                   plot.title.position = "plot",
                   plot.subtitle = formt_text(markdown = markdown,
                                              hjust = 0.5,
                                              size = 12*text_resize,
                                              color = "gray50"),
                   plot.caption =  formt_text(markdown = markdown,
                                              color = "gray50",
                                              size = 10*text_resize,
                                              hjust = 0),
                   panel.grid = ggplot2::element_line(linetype = 2,
                                                      size = 0.3,
                                                      color = "gray90"),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.key.width= ggplot2::unit(0.7, 'cm'),
                   legend.margin = ggplot2::margin(0,0,0,0),
                   legend.spacing = ggplot2::unit(0.2, "cm"),
                   legend.position = "top",
                   strip.background = ggplot2::element_rect(fill = "gray95",
                                                            linetype = "blank"),
                   panel.border = ggplot2::element_rect(color = "gray95",
                                                        fill = NA),
                   rect = ggplot2::element_rect(fill = "transparent")) +
    ggplot2::theme(...)
}


#' Paletas de colores
#'
#' Es una lista de las paletas
#'
#' @format list
#' @export
paletas_jmr <- list(
  `general` = c("#f94144", "#277DA1", "#F9C74F", "#43AA8B"),
  `generalextend` = c("#277DA1", "#f94144", "#F9C74F", "#43AA8B", "#FFAC41",
                      "#58F7B4", "#C6001C", "#AED8FF", "#16A1CD", "#b2e061",
                      "#bd7ebe", "#ffee65", "#beb9db", "#e76f51","#fdcce5", 
                      "#8bd3c7","#264653","#f4a261", "#d5bdaf", "#335c67",
                      "#9e2a2b"),
  `imp1`  = c("#19749F", "#FF3E2B", "#008080", "#FEC260"),
  `imp2`  = c("#1E81A2", "#039176", "#FFAC41", "#FF483B"),
  `bi`  = c("#C6001C", "#FF5454", "#FF483B",
            "#15607A", "#1E81A2", "#16A1CD"),
  `rainbow` = c("#f94144", "#f3722c", "#f8961e", "#f9844a", "#f9c74f",
                "#90be6d", "#43aa8b", "#4d908e", "#577590", "#277da1"),
  `bi_2` = c("#001219","#005f73","#0a9396","#94d2bd","#e9d8a6",
             "#ee9b00","#ca6702","#bb3e03","#ae2012","#9b2226"),
  `order_red`  = c("#C6001C", "#FF5454", "#FF483B"),
  `order_blue`  = c("#15607A",
                    "#1E81A2",
                    "#16A1CD",
                    "#AED8FF"),
  `order_green`  = c("#039176",
                     "#0BBB9F",
                     "#00DCA5",
                     "#58F7B4",
                     "#C5FFD7"),
  `order_yellow`  = c("#FF8902",
                      "#FFAC41",
                      "#FFBE49"),
  `multiple`  = c("#15607A",
                  "#1E81A2",
                  "#16A1CD",
                  "#AED8FF",
                  "#C6001C",
                  "#FF5454",
                  "#FF483B",
                  "#FF8902",
                  "#FFAC41",
                  "#FFBE49",
                  "#039176",
                  "#0BBB9F",
                  "#00DCA5",
                  "#58F7B4",
                  "#C5FFD7")
)


#' Color interpolacion de paletas
#'
#' Esta función genera nuevos colores cuando es necesario y determina orde.
#'
#' @param palette Determina qué paleta tomar. Puede ser `r names(paletas_jmr)`
#' @param reverse Determina el orden de la paleta.
#' @param ... Demás elementos correspondientes a grDevices::colorRampPalette()
#'
#' @return regresa una función con un argumento integer (el número de colores)
#' y un vector de colores.
#' @export
jmr_pal <- function(palette = "general", reverse = FALSE, ...) {
  pal <- paletas_jmr[[palette]]
  
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
jmr_pal_manual <- function(n, palette = "general", reverse = FALSE, ...) {
  pal <- paletas_jmr[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  grDevices::colorRampPalette(pal, ...)(n)
  
}

#' Función para añadir colores de  a gráfica
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
scale_color_jmr <- function(palette = "general", discrete = TRUE, reverse = FALSE, ...) {
  pal <- jmr_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    ggplot2::discrete_scale("colour", palette, palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}


#' Función para añadir fill  a gráfica
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
scale_fill_jmr <- function(palette = "general", discrete = TRUE, reverse = FALSE, ...) {
  pal <- jmr_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    ggplot2::discrete_scale("fill", palette, palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}


#' @title Set Custom Plot Theme
#' @description This function configures a customized theme for ggplot2 plots, setting locale options, disabling scientific notation, and applying specified theme settings to specified ggplot2 geometric objects.
#' @param geoms A character vector specifying the ggplot2 geometric objects for which to set default styles. Defaults to `c("bar", "col", "area", "point", "boxplot", "histogram")`.
#' @param ... Additional arguments passed to `theme_jmr`, allowing customization of the plot theme.
#' @details This function sets a customized ggplot2 theme by applying `theme_jmr` globally and configuring color and fill palettes based on `dis_palette`. It also sets locale options if running on a macOS system, disables scientific notation, and updates the default aesthetics of specified ggplot2 geoms to use colors from the `paletas_jmr` palette with adjusted transparency levels.
#' @return This function is used for its side effects of setting theme and display options. It does not return a value.
#' @export
set_mytheme <- function(geoms = c("bar", "col", "area",
                                  "point", "boxplot", "bin", 
                                  "identity", "line"),
                        dis_pal = "generalextend",
                        con_pal = "order_blue",
                        ...) {
  
  ## Specify locale ----
  if(Sys.info()[['sysname']] == "Darwin") Sys.setlocale("LC_ALL", "es_ES.UTF-8")
  
  ## Disable scientific notation ----
  options(scipen = 999)
  
  # Set theme
  ggplot2::theme_set(theme_jmr(...))
  dis_pal_list <- paletas_jmr[[dis_pal]]
  
  options(ggplot2.discrete.colour = dis_pal_list,
          ggplot2.discrete.fill = dis_pal_list)
  options(ggplot2.continuous.fill = ~scale_fill_jmr(discrete = F,
                                                    palette = con_pal))
  
  purrr::walk(geoms,
              function(.x){
                
                alpha <- dplyr::case_when(.x %in% c("col", "bar") ~ 0.9,
                                          .x %in% c("point", "identity") ~ 0.7,
                                          .x %in% c("line", "function") ~ 1,
                                          TRUE ~ 0.8)
                
                if(.x %in% c("bin", "identity")) {
                  ggplot2::update_stat_defaults(
                    .x,   
                    list(fill = dis_pal_list[1],
                         alpha = alpha))
                } else if (.x %in% c("point", "line", "function")) {
                  ggplot2::update_geom_defaults(
                    .x,   
                    list(color = dis_pal_list[1],
                         alpha = alpha))
                } else {
                  ggplot2::update_geom_defaults(
                    .x,   
                    list(fill = dis_pal_list[1],
                         alpha = alpha))
                }
              })
}
