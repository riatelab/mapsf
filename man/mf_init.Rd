% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mf_init.R
\name{mf_init}
\alias{mf_init}
\title{Initialize a map with a specific extent}
\usage{
mf_init(x, expandBB = rep(0, 4), theme)
}
\arguments{
\item{x}{object of class \code{sf}, \code{sfc} or \code{Raster}}

\item{expandBB}{fractional values to expand the bounding box with, in each
direction (bottom, left, top, right)}

\item{theme}{apply a theme from \code{mf_theme}}
}
\value{
No return value, a map is initiated.
}
\description{
Plot an invisible layer with the extent of a spatial object.
}
\examples{
mtq <- mf_get_mtq()
target <- mtq[30, ]
mf_init(target)
mf_map(mtq, add = TRUE)
}
