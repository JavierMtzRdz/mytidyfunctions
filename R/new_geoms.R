#' Valores absolutos abarcados
#'
#' Esta función te muestra en valores absolutos los montos recorridos por la colúmna
#' o vector evaluados.
#'
#' @param variable Vector o colúmna de datos a partir del cuál se calculan los datoss
#'
#' @return Valor absoluto de los valores absolutos (positivos y negativos) que recorre
#' el vector o colúmna.
#' @export
width_bar <- function(variable) {
  ancho_barras <- ifelse(max(variable) <= 0,
    abs(min(variable)),
    max(variable)
  ) +
    ifelse(min(variable) < 0,
      ifelse(max(variable) <= 0,
        0,
        abs(min(variable))
      ), 0
    )

  ancho_barras <- ifelse(ancho_barras == 0, 1, ancho_barras)

  return(ancho_barras)
}

#' Ggproto del geom text para barras
#'
#' Esta función es la gase de geom_text_bi()
#'
#' @param data input1
#'
#' @return Una nueva capa de texto para una gráfica
#' @export
#'

GeomTextBi <- ggplot2::ggproto("GeomTextBi", ggplot2::Geom,
  required_aes = c("x", "y", "label"),
  default_aes = ggplot2::aes(
    colour = NA,
    size = 3.88, angle = 0,
    hjust = 0.5, vjust = 0.5,
    hdist = 0, vdist = 0,
    alpha = NA, family = "",
    fontface = "bold", lineheight = 1.2,
    position_hor = T,
    percent_change = 0.25,
    color_black = "grey15",
    color_light = "grey95"
  ),
  draw_panel = function(data, panel_params, coord, parse = FALSE,
                        na.rm = FALSE, check_overlap = FALSE,
                        position_hor = T) {
    lab <- data$label

    if (parse) {
      lab <- parse_safe(as.character(lab))
    }

    values <- data$y

    data <- coord$transform(data, panel_params)

    if (any(is.na(data$colour))) {
      col_black <- data$colour_black[1]
      col_light <- data$colour_light[1]
    }


    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y, data$x, data$angle)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x, data$y, data$angle)
    }

    if (is.null(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x, data$y, data$angle)
    }

    if (is.null(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x, data$y, data$angle)
    }


    if (position_hor) {
      width <- width_bar(panel_params$y.range)

      data$vjust <- ifelse((values >= 0 &
        values / width >= data$percent_change) |
        (values < 0 &
          abs(values) / width < data$percent_change),
      data$vjust + 0.9 - data$vdist - ifelse(data$angle == 90, 0.9, 0),
      data$vjust - 0.9 + data$vdist + ifelse(data$angle == 90, 0.9, 0)
      )

      data$hjust <- ifelse((values >= 0 &
        values / width >= data$percent_change) |
        (values < 0 &
          abs(values) / width < data$percent_change),
      data$hjust + data$hdist + ifelse(data$angle == 90, 0.6, 0),
      data$hjust - data$hdist - ifelse(data$angle == 90, 0.6, 0)
      )


      if (any(is.na(data$colour))) {
        data$colour <- ifelse(abs(values) / width < data$percent_change,
          col_black,
          col_light
        )
      }
    } else {
      width <- width_bar(panel_params$x.range)

      data$hjust <- ifelse((values >= 0 &
        values / width >= data$percent_change) |
        (values < 0 &
          abs(values) / width < data$percent_change),
      data$hjust + 0.6 - data$hdist - ifelse(data$angle == -90, 0.6, 0),
      data$hjust - 0.6 + data$hdist + ifelse(data$angle == -90, 0.6, 0)
      )


      data$vjust <- ifelse((values >= 0 &
        values / width >= data$percent_change) |
        (values < 0 &
          abs(values) / width < data$percent_change),
      data$vjust - 0.05 + data$vdist + ifelse(data$angle == -90, 0.8, 0),
      data$vjust + 0.05 - data$vdist - ifelse(data$angle == -90, 0.8, 0)
      )


      if (any(is.na(data$colour))) {
        data$colour <- ifelse(abs(values) / width < data$percent_change,
          col_black,
          col_light
        )
      }
    }

    grid::textGrob(
      lab,
      data$x, data$y,
      default.units = "native",
      hjust = data$hjust, vjust = data$vjust,
      rot = data$angle,
      gp = grid::gpar(
        col = ggplot2::alpha(data$colour, data$alpha),
        fontsize = data$size * .pt,
        fontfamily = data$family,
        fontface = data$fontface,
        lineheight = data$lineheight
      ),
      check.overlap = check_overlap
    )
  },
  draw_key = ggplot2::draw_key_text
)


#' Geom text para barras
#'
#' Esta función sirve para añadir fácilmente los valores para los barras de variación
#' en valores negativos y positivos. Sólo se esplican los parámetros que difieren de
#' geom_text() normal
#'
#' @param data input1
#'
#' @return Una nueva capa de texto para una gráfica
#' @export
geom_text_bi <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                         parse = FALSE,
                         nudge_x = 0,
                         nudge_y = 0,
                         check_overlap = FALSE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         position_hor = T,
                         percent_change = 0.25,
                         color_black = "grey15",
                         color_light = "grey95",
                         hdist = 0, vdist = 0) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      cli::cli_abort(c(
        "both {.arg position} and {.arg nudge_x}/{.arg nudge_y} are supplied",
        "i" = "Only use one approach to alter the position"
      ))
    }

    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTextBi,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      position_hor = position_hor,
      percent_change = percent_change,
      color_black = color_black,
      color_light = color_light,
      hdist = hdist, vdist = vdist,
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' Geom para generar segmentos sobre cada valor.
#'
#' Esta función genera segmentos para cada valor de x y así mostrar cierto nivel.
#'
#' @param data input1
#'
#' @return Una nueva capa de segmentos para una gráfica.
#' @export
geom_segment_point <- function(point_var_x,
                               point_var_y,
                               color_point = NULL,
                               mapping = NULL, data = NULL,
                               stat = "identity", position = "identity",
                               point_dist = 0.4,
                               ..., arrow = NULL, arrow.fill = NULL,
                               lineend = "butt", linejoin = "round",
                               na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  if (!missing(color_point)) {
    var_color <- rlang::enquo(color_point)

    variable_x <- rlang::enquo(point_var_x)

    variable_y <- rlang::enquo(point_var_y)

    mapping_seg <- ggplot2::aes(
      x = !!variable_x - point_dist,
      xend = !!variable_x + point_dist,
      y = !!variable_y,
      yend = !!variable_y,
      color = !!var_color
    )

    mapping_point_1 <- ggplot2::aes(
      x = !!variable_x + point_dist,
      y = !!variable_y,
      color = !!var_color
    )

    mapping_point_2 <- ggplot2::aes(
      x = !!variable_x - point_dist,
      y = !!variable_y,
      color = !!var_color
    )
  } else {
    variable_x <- rlang::enquo(point_var_x)

    variable_y <- rlang::enquo(point_var_y)

    mapping_seg <- ggplot2::aes(
      x = !!variable_x - point_dist,
      xend = !!variable_x + point_dist,
      y = !!variable_y,
      yend = !!variable_y
    )

    mapping_point_1 <- ggplot2::aes(
      x = !!variable_x + point_dist,
      y = !!variable_y
    )

    mapping_point_2 <- ggplot2::aes(
      x = !!variable_x - point_dist,
      y = !!variable_y
    )
  }

  list(
    ggplot2::layer(
      data = data, mapping = mapping_seg, stat = stat, geom = GeomSegment,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(
        arrow = arrow, arrow.fill = arrow.fill,
        lineend = lineend, linejoin = linejoin, na.rm = na.rm,
        ...
      )
    ),
    ggplot2::layer(
      data = data, mapping = mapping_point_1, stat = stat, geom = GeomPoint,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    ),
    ggplot2::layer(
      data = data, mapping = mapping_point_2, stat = stat, geom = GeomPoint,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  )
}

#' Geom para generar segmentos sobre cada valor en posición vertical.
#'
#' Esta función genera segmentos para cada valor de x y así mostrar cierto nivel.
#'
#' @param data input1
#'
#' @return Una nueva capa de segmentos para una gráfica.
#' @export
geom_segment_pointv <- function(point_var_x,
                                point_var_y,
                                color_point = NULL,
                                mapping = NULL, data = NULL,
                                stat = "identity", position = "identity",
                                point_dist = 0.4,
                                ..., arrow = NULL, arrow.fill = NULL,
                                lineend = "butt", linejoin = "round",
                                na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  if (!missing(color_point)) {
    var_color <- rlang::enquo(color_point)

    variable_x <- rlang::enquo(point_var_x)

    variable_y <- rlang::enquo(point_var_y)

    mapping_seg <- ggplot2::aes(
      x = !!variable_x - point_dist,
      xend = !!variable_x + point_dist,
      y = !!variable_y,
      yend = !!variable_y,
      color = !!var_color
    )

    mapping_point_1 <- ggplot2::aes(
      x = !!variable_x + point_dist,
      y = !!variable_y,
      color = !!var_color
    )

    mapping_point_2 <- ggplot2::aes(
      x = !!variable_x - point_dist,
      y = !!variable_y,
      color = !!var_color
    )
  } else {
    variable_x <- rlang::enquo(point_var_x)

    variable_y <- rlang::enquo(point_var_y)

    mapping_seg <- ggplot2::aes(
      x = !!variable_x,
      xend = !!variable_x,
      y = !!variable_y - point_dist,
      yend = !!variable_y + point_dist
    )

    mapping_point_1 <- ggplot2::aes(
      x = !!variable_x,
      y = !!variable_y + point_dist
    )

    mapping_point_2 <- ggplot2::aes(
      x = !!variable_x,
      y = !!variable_y - point_dist
    )
  }

  list(
    ggplot2::layer(
      data = data, mapping = mapping_seg, stat = stat, geom = GeomSegment,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(
        arrow = arrow, arrow.fill = arrow.fill,
        lineend = lineend, linejoin = linejoin, na.rm = na.rm,
        ...
      )
    ),
    ggplot2::layer(
      data = data, mapping = mapping_point_1, stat = stat, geom = GeomPoint,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    ),
    ggplot2::layer(
      data = data, mapping = mapping_point_2, stat = stat, geom = GeomPoint,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  )
}

#' Función para guardar gráficas en múltiples formatos
#'
#' Esta función guarda las gráfics en los formatos señalados
#'
#' @param format formatos en los que se quiere exportar
#' @param path_name Ubicación y nombre de la imagen guardada
#'
#' @return Guarda una gráfica
#' @export
#'
ggsave_mult <- function(format = "png",
                        path_name,
                        .plot = ggplot2::last_plot(),
                        type_margin = NULL,
                        width = 200,
                        height = 120,
                        units = "mm",
                        dpi = 300,
                        scale = 1,
                        ...) {
  if (is.null(type_margin)) {
    width <- width
    height <- height
    units <- units
    dpi <- dpi
    scale <- scale
  } else {
    if (type_margin == "cuadrado") {
      width <- 1200
      height <- 1200
      units <- "px"
      scale <- 4 / 2
      dpi <- 840 / 2
    } else if (type_margin == "largo") {
      width <- 1200
      height <- 1700
      units <- "px"
      scale <- 4 / 2
      dpi <- 840 / 2
    } else {
      warning("No tiene un tipo de marge aceptable")
    }
  }
  for (i in format) {
    if (i == "xlsx") {
      if (ggplot2::is.ggplot(.plot %>% .[[1]])) {
        .plot %>%
          .[[1]] %>%
          .$data %>%
          writexl::write_xlsx(paste0(
            path_name,
            ".",
            i
          ))
      } else {
        .plot %>%
          .$data %>%
          writexl::write_xlsx(paste0(
            path_name,
            ".",
            i
          ))
      }
    }
    if (i == "csv") {
      if (ggplot2::is.ggplot(.plot %>% .[[1]])) {
        .plot %>%
          .[[1]] %>%
          .$data %>%
          readr::read_csv(paste0(
            path_name,
            ".",
            i
          ))
      } else {
        .plot %>%
          .$data %>%
          readr::read_csv(paste0(
            path_name,
            ".",
            i
          ))
      }
    }
    if (!(i %in% c("xlsx"))) {
      ggplot2::ggsave(
        plot = .plot,
        paste0(path_name, ".", i),
        bg = "transparent",
        width = width,
        height = height,
        units = units,
        dpi = dpi,
        scale = scale,
        ...
      )
    }
  }
}

#' Función para determinar qupe función usar
#'
#' Esa función determina si usar formato markdawn o texto normal.
#'
#' @param markdown booleano para determinar si usar la función de base o la
#' de {{ggtext}}.
#' @param ... los elementos correspondientes al formato de ggplot2::element_text.
#'
#' @return una función con base en la elección
#' @export
formt_text <- function(markdown = F,
                       ...) {
  f <- if (markdown) {
    ggtext::element_markdown
  } else {
    ggplot2::element_text
  }
  f(...)
}

#' Tema inicial con formato general
#'
#' Esta resume las funciones de estilo utilizadas en mis gráficas
#'
#' @param ... cambios en el tema
#'
#' @return Tema para gráficas de ggplot
#' @export
#'
mi_tema <- function(...,
                    markdown = F) {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Lato"),
      axis.line = ggplot2::element_line(size = 0.3),
      plot.title = formt_text(
        markdown = markdown,
        hjust = 0.5,
        size = 14, face = "bold",
        color = "grey20"
      ),
      plot.title.position = "plot",
      plot.subtitle = formt_text(
        markdown = markdown,
        hjust = 0.5,
        size = 12,
        color = "gray50"
      ),
      plot.caption = formt_text(
        markdown = markdown,
        color = "gray50",
        size = 10,
        hjust = 0
      ),
      panel.grid = ggplot2::element_line(
        linetype = 2,
        size = 0.3,
        color = "gray90"
      ),
      # panel.grid = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      legend.key.width = unit(0.7, "cm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.spacing = unit(0, "cm"),
      strip.background = ggplot2::element_rect(
        fill = "gray95",
        linetype = "blank"
      ),
      panel.border = ggplot2::element_rect(
        color = "gray95",
        fill = NA
      ),
      rect = ggplot2::element_rect(fill = "transparent")
    ) +
    ggplot2::theme(...)
}


#' Tema inicial con formato html
#'
#' Esta resume las funciones de estilo utilizadas en mis gráficas
#' con estilo html
#'
#' @param ... cambios en el tema
#'
#' @return Tema en html para gráficas de ggplot
#' @export
mi_tema_html <- function(...) {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Lato"),
      axis.line = ggplot2::element_line(size = 0.3),
      plot.title = ggtext::element_markdown(
        hjust = 0.5,
        size = 14, face = "bold",
        color = "grey20"
      ),
      plot.subtitle = ggtext::element_markdown(
        hjust = 0.5,
        size = 12,
        color = "gray50"
      ),
      plot.caption = ggtext::element_markdown(
        color = "gray50",
        size = 9,
        hjust = 0
      ),
      panel.grid = ggplot2::element_line(
        linetype = 2,
        size = 0.3,
        color = "gray90"
      ),
      # panel.grid = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(
        fill = "gray95",
        linetype = "blank"
      ),
      panel.border = ggplot2::element_rect(
        color = "gray95",
        fill = NA
      ),
      rect = ggplot2::element_rect(fill = "transparent")
    ) +
    ggplot2::theme(...)
}


#' ggplot layer before last one
#'
#' ...
#'
#' @param plot
#'
#' @return plot
#' @export
`-.gg` <- function(plot, layer) {
  if (missing(layer)) {
    stop("Cannot use `-.gg()` with a single argument. Did you accidentally put - on a new line?")
  }
  if (!ggplot2::is.ggplot(plot)) {
    stop("Need a plot on the left side")
  }
  plot$layers <- c(layer, plot$layers)
  plot
}



#' insertar ggplot layer
#'
#' ...
#'
#' @param plot
#'
#' @return plot
#' @export
insertLayer <- function(P, after = 0, ...) {
  #  P     : Plot object
  # after  : Position where to insert new layers, relative to existing layers
  #  ...   : additional layers, separated by commas (,) instead of plus sign (+)

  if (after < 0) {
    after <- after + length(P$layers)
  }

  if (!length(P$layers)) {
    P$layers <- list(...)
  } else {
    P$layers <- append(P$layers, list(...), after)
  }

  return(P)
}

#' Logaritmic transformation with exponentil for negatives
#'
#' ...
#'
#' @param x Values
#'
#' @return Values converted
#' @export
#'
log_both <- function(x) {
  ifelse(x == 0, 0, log(abs(x)) * sign(x))
}

exp_both <- function(x) {
  exp(abs(x)) * sign(x)
} # this is the inverse of log_both

log_both_trans <- function() {
  scales::trans_new(
    name = "log_both",
    transform = log_both,
    inverse = exp_both
  )
}
