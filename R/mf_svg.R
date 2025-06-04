#' @title Export a map in svg format
#' @name mf_svg
#' @description Export a map with the extent of a spatial object.\cr
#' The map is exported in SVG format.\cr
#' If only one of \code{width} or \code{height} is set, \code{mf_export} uses
#' the width/height ratio of \code{x} bounding box to find a matching ratio for
#' the export.\cr
#' Use \code{dev.off} to finish the export (see Examples).
#'
#'
#' @param x object of class \code{sf}, \code{sfc} or \code{SpatRaster}
#' @param expandBB fractional values to expand the bounding box with, in each
#' direction (bottom, left, top, right)
#' @param filename path to the exported file.
#' @param width width of the figure (inches)
#' @param height height of the figure (inches)
#' @param svglite if TRUE, the export is done with the \code{svglite} package
#' if installed
#' @param ... further parameters
#' @export
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
        paste0("'svglite' is not installed and will not be used to export",
               "the map.")
      )
      svg(filename, width = ratio[1], height = ratio[2], ...)
    }
  } else {
    svg(filename, width = ratio[1], height = ratio[2], ...)
  }

}
