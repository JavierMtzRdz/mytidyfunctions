% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/style.R
\name{jmr_pal}
\alias{jmr_pal}
\title{Color interpolacion de paletas}
\usage{
jmr_pal(palette = "general", reverse = FALSE, ...)
}
\arguments{
\item{palette}{Determina qué paleta tomar. Puede ser `r names(paletas_jmr)`}

\item{reverse}{Determina el orden de la paleta.}

\item{...}{Demás elementos correspondientes a grDevices::colorRampPalette()}
}
\value{
regresa una función con un argumento integer (el número de colores)
y un vector de colores.
}
\description{
Esta función genera nuevos colores cuando es necesario y determina orde.
}
