#' Geometric Segmented Points (geom_segpoint)
#'
#' Add segmented points to a ggplot2 plot, combining segments and points to
#' represent data points. This geom is particularly useful for adding marks 
#' over the geom_col to provide additional information without the need for 
#' a extra bar.
#'
#' @param mapping A list specifying how variables in the data are mapped 
#' to aesthetic properties (e.g., x, y, color, size) of the geom.
#' @param data A data frame or tibble containing the data to be visualized.
#' @param stat The statistical transformation to be applied to the data 
#' before plotting.
#' @param position The method used to position the data points on the plot.
#' @param ... Additional parameters and arguments to be passed to other 
#' functions or layers.
#' @param arrow Optional arrow specification for the segments, allowing you 
#' to customize the arrow's appearance.
#' @param width The width of the segments drawn from the data points. Default 
#' is 0.4.
#' @param arrow.fill The fill color of the arrow. If not provided, it defaults 
#' to the color specified in the plot.
#' @param na.rm A logical value indicating whether to remove missing values 
#' from the data before plotting.
#' @param show.legend A logical value indicating whether to show the legend 
#' for this layer.
#' @param inherit.aes A logical value indicating whether to inherit the 
#' aesthetics from the parent ggplot2 object.
#'
#' @return A ggplot2 layer representing the geom \code{\link{GeomSegPoint}}.
#'
#' @examples
#' # Basic example
#' set.seed(1234)
#' db <- data.frame(category = 2001:2020,
#'                  val1 = rnorm(20, 100, 10),
#'                  val2 = rnorm(20, 100, 10))
#' ggplot(data = db, aes(x = category, y = val1)) +
#'   geom_col() +
#'   geom_segpoint(aes(y = val2))
#'
#' @seealso \code{\link{GeomSegPoint}} for the geom object and additional details.
#'
#' @export
geom_segpoint <- function(mapping = NULL, data = NULL,
                          stat = "identity", 
                          position = "identity",
                          ...,
                          arrow = NULL,
                          width = 0.4,
                          arrow.fill = NULL,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSegPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      width = width,
      arrow.fill = arrow.fill,
      na.rm = na.rm,
      ...
    )
  )
}

#' Custom Key Drawing Function for GeomSegPoint (draw_key_segpoint)
#'
#' Custom key drawing function for the \code{\link{GeomSegPoint}} geom.
#' This function is responsible for drawing the legend key for the segments 
#' and points in the plot.
#'
#' @param data A data frame containing information about the legend key to 
#' be drawn.
#' @param params A list of parameters specifying aesthetics and properties 
#' of the geom.
#' @param size The size of the legend key.
#' @importFrom rlang %||%
#'
#' @return A \code{grid::grobTree} representing the legend key for the
#' \code{\link{GeomSegPoint}} geom.
#'
#' @export
draw_key_segpoint <- function(data, params, size) {
  
  if (is.null(data$linetype)) {
    data$linetype <- 0
  }
  else {
    data$linetype[is.na(data$linetype)] <- 0
  }
  if (is.null(data$shape)) {
    data$shape <- 19
  }
  else if (is.character(data$shape)) {
    data$shape <- ggplot2:::translate_shape_string(data$shape)
  }
  stroke_size <- data$stroke %||% 0.5
  stroke_size[is.na(stroke_size)] <- 0
  grid::grobTree(
    grid::segmentsGrob(0.15, 0.5, 0.85, 0.5, 
                       gp = grid::gpar(col = alpha(data$colour %||% 
                                                     data$fill %||% 
                                                     "black", 
                                                   data$alpha),
                                       fill = alpha(params$arrow.fill %||% 
                                                      data$colour %||% 
                                                      data$fill %||% 
                                                      "black", 
                                                    data$alpha), 
                                       lwd = (data$linewidth %||% 0.5) * .pt,
                                       lty = data$linetype %||% 1, 
                                       lineend = params$lineend %||% "butt"), 
                       arrow = params$arrow),
    grid::pointsGrob(c(0.15, 0.85), c(0.5,
                                      0.5), 
                     pch = data$shape, 
                     gp = grid::gpar(col = alpha(data$colour %||% 
                                                   "black", data$alpha), 
                                     fill = alpha(data$fill %||% "black", 
                                                  data$alpha), 
                                     fontsize = (data$size %||% 1.5) * .pt + 
                                       stroke_size * .stroke/2, 
                                     lwd = stroke_size * .stroke/2))
  )
}

#' GeomSegPoint object
#'
#' Custom ggproto object that defines the properties and behavior of the
#' \code{geom_segpoint} function. The \code{GeomSegPoint} object inherits 
#' from the \code{Geom} base class and is used to create the segments and 
#' points for the \code{geom_segpoint} geom.
#'
#' @seealso \code{\link[ggplot2]{Geom}} for details about the base class.
#'
#' @field required_aes A character vector specifying the aesthetics required 
#' by this geom. In this case, "x" and "y" are required to represent the 
#' data points' positions.
#' @field non_missing_aes A character vector specifying other aesthetics that 
#' can be used with this geom, even if they have missing values.
#' @field default_aes Aesthetic mappings that are applied by default to the 
#' geom, in case the user does not explicitly specify them.
#' @importFrom rlang %||%
#'
#' @export
GeomSegPoint <- ggproto("GeomSegPoint", Geom,
                        required_aes = c("x", "y"),
                        non_missing_aes = c("linetype", "linewidth", "shape",
                                            "size"),
                        default_aes = aes(colour = "black", 
                                          linewidth = 0.5,
                                          width = 0.4,
                                          shape = 19,
                                          stroke = 0.5,
                                          size = 0.5,
                                          linetype = 1, 
                                          alpha = NA),
                        draw_panel = function(self, data, panel_params, 
                                              coord, arrow = NULL,
                                              arrow.fill = NULL,
                                              width = 0.4,
                                              na.rm = FALSE) {
                          data$xend <- data$x - width
                          data$x <- data$x + width
                          data$yend <- data$y
                          data <- ggplot2:::check_linewidth(data, snake_class(self))
                          data <- remove_missing(data, na.rm = na.rm,
                                                 c("x", "y", "width", "linetype", "linewidth", "shape"),
                                                 name = "geom_segpoint"
                          )
                          
                          if (ggplot2:::empty(data)) return(zeroGrob())
                          
                          if (coord$is_linear()) {
                            
                            
                            coord <- coord$transform(data, panel_params)
                            stroke_size <- coord$stroke
                            stroke_size[is.na(stroke_size)] <- 0
                            arrow.fill <- arrow.fill %||% coord$colour
                            return(grid::gTree(
                              children = grid::gList(
                                grid::segmentsGrob(coord$x, coord$y, coord$xend, coord$yend,
                                                   default.units = "native",
                                                   gp = grid::gpar(
                                                     col = alpha(coord$colour, coord$alpha),
                                                     fill = alpha(arrow.fill, coord$alpha),
                                                     lwd = coord$linewidth * .pt,
                                                     lty = coord$linetype
                                                   ),
                                                   arrow = arrow
                                ),
                                grid::pointsGrob(x = coord$x,
                                                 y = coord$y,
                                                 pch = coord$shape,
                                                 gp = grid::gpar(
                                                   col = alpha(coord$colour, coord$alpha),
                                                   fill = alpha(arrow.fill, coord$alpha),
                                                   fontsize = coord$size * .pt + stroke_size * .stroke * 1.7,
                                                   lwd = coord$stroke * .stroke
                                                 )),
                                grid::pointsGrob(x = coord$xend,
                                                 y = coord$yend,
                                                 pch = coord$shape,
                                                 gp = grid::gpar(
                                                   col = alpha(coord$colour, coord$alpha),
                                                   fill = alpha(arrow.fill, coord$alpha),
                                                   fontsize = coord$size * .pt + stroke_size * .stroke * 1.7,
                                                   lwd = coord$stroke * .stroke
                                                 ))
                              )
                            )
                            )
                            
                            
                          }
                          
                          data$group <- 1:nrow(data)
                          starts <- subset(data, select = c(-xend, -yend))
                          ends <- rename(subset(data, select = c(-x, -y)), 
                                         c("xend" = "x", "yend" = "y"))
                          
                          pieces <- vec_rbind0(starts, ends)
                          pieces <- pieces[order(pieces$group),]
                          
                          GeomPath$draw_panel(pieces, panel_params,
                                              coord, arrow = arrow,
                                              lineend = lineend)
                          
                        },
                        
                        draw_key = draw_key_segpoint,
                        
                        rename_size = TRUE
)


