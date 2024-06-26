% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new_geoms.R
\name{ggsave_mult}
\alias{ggsave_mult}
\title{Función para guardar gráficas en múltiples formatos}
\usage{
ggsave_mult(
  format = "png",
  path_name,
  .plot = ggplot2::last_plot(),
  type_margin = NULL,
  width = 200,
  height = 120,
  units = "mm",
  dpi = 300,
  scale = 1,
  ...
)
}
\arguments{
\item{format}{formatos en los que se quiere exportar}

\item{path_name}{Ubicación y nombre de la imagen guardada}
}
\value{
Guarda una gráfica
}
\description{
Esta función guarda las gráfics en los formatos señalados
}
