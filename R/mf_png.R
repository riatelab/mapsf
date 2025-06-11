#' @title Export a map in PNG format
#' @name mf_png
#' @description Export a map with the extent of a spatial object in PNG format.
#'
#' PNG is a raster graphics file format and PNG export should be used for
#' maps that do not require further modification.
#'
#' If `width` is specified, then `height` is deduced from the width/height
#' ratio of `x`. Alternatively, if `height` is specified, then `width`
#' is deduced from the width/height ratio of `x`.
#' This helps to produce maps without too much wasted space.
#'
#' Use \code{dev.off} to finish the export (see Examples).
#' @md
#' @param x object of class \code{sf}, \code{sfc} or \code{SpatRaster}
#' @param expandBB fractional values to expand the bounding box with, in each
#' direction (bottom, left, top, right)
#' @param filename path to the exported file
#' @param width width of the figure (pixels)
#' @param height height of the figure (pixels)
#' @param ... further parameters
#' @export
#' @return No return value, a PNG device is initiated.
#' @examples
#' mtq <- mf_get_mtq()
#' (filename <- tempfile(fileext = ".png"))
#' mf_png(mtq, filename = filename)
#' mf_map(mtq)
#' mf_title()
#' dev.off()
mf_png <- function(x, filename = "map.png",
                   width, height,
                   expandBB = rep(0, 4),
                   ...) {
  test_ratio_input(x)
  bb <- x_to_bb(x = x, expandBB = expandBB)
  mar <- get_mar()
  ratio <- bb_to_ratio(bb = bb, mar = mar)

  if (missing(width) && missing(height)) {
    width <- 672
  }

  if (missing(height)) {
    height <- round(ratio[2] *  width / ratio[1], 0)
  }

  if (missing(width)) {
    width <- round(ratio[1] * height / ratio[2], 0)
  }

  if (isTRUE(capabilities("cairo"))) {
    png(filename = filename, width = width, height = height, res = 96,
        type = "cairo-png", ...
    )
  } else {

    png(filename = filename, width = width, height = height, res = 96, ...)
  }

}
