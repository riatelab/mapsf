#' @title Export a map in SVG format
#' @name mf_svg
#' @description Export a map with the extent of a spatial object in SVG format.
#'
#' SVG export is the perfect solution for editing maps with desktop vector
#' graphics software. SVG is a vector graphics file format.
#'
#' If `width` is specified, then `height` is deduced from the width/height
#' ratio of `x`. Alternatively, if `height` is specified, then `width`
#' is deduced from the width/height ratio of `x`.
#' This helps to produce maps without too much wasted space.
#'
#' Use \code{dev.off} to finish the export (see Examples).
#'
#' @md
#' @param x object of class \code{sf}, \code{sfc} or \code{SpatRaster}
#' @param expandBB fractional values to expand the bounding box with, in each
#' direction (bottom, left, top, right)
#' @param filename path to the exported file
#' @param width width of the figure (inches)
#' @param height height of the figure (inches)
#' @param svglite if TRUE, the export is done with the \code{svglite} package
#' if it is installed (see Details)
#' @param ... further parameters
#' @export
#' @details
#' The default driver for building SVG files, `grDevices::svg()`, has
#' limitations regarding speed, file size, editability, and font support.
#' The `svglite` package aims to solve these issues but it is not lightweight
#' in terms of dependencies, so it is not imported by `mapsf`, but rather
#' suggested.
#'
#' However, we strongly recommend its use if the aim is to edit the maps
#' after export.
#'
#' @return No return value, an SVG device is initiated.
#' @examples
#' mtq <- mf_get_mtq()
#' (filename <- tempfile(fileext = ".svg"))
#' mf_svg(mtq, filename = filename)
#' mf_map(mtq)
#' mf_title()
#' dev.off()
mf_svg <- function(x, filename = "map.svg",
                   width, height,
                   expandBB = rep(0, 4),
                   svglite = TRUE,
                   ...) {
  test_ratio_input(x)
  bb <- x_to_bb(x = x, expandBB = expandBB)
  mar <- get_mar()
  ratio <- bb_to_ratio(bb = bb, mar = mar, width = width, height = height)
  if (isTRUE(svglite)){
    if (requireNamespace("svglite", quietly = TRUE)) {
      svglite::svglite(filename, width = ratio[1], height = ratio[2],
                       fix_text_size = FALSE, ...)
    } else {
      message(
        paste0("'svglite' is not installed.")
      )
      svg(filename, width = ratio[1], height = ratio[2], ...)
    }
  } else {
    svg(filename, width = ratio[1], height = ratio[2], ...)
  }

}
