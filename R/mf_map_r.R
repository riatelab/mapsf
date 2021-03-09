#' @title Plot a raster
#' @description Plot a raster object over a map (RasterLayer and RasterBrick).
#' @name mf_raster
#' @param x a RasterLayer or RasterBrick object
#' @param add whether to add the layer to an existing plot (TRUE) or
#' not (FALSE).
#' @param ... bgalpha, interpolate, or other arguments passed to be passed to
#' \code{\link[raster:plotRGB]{plotRGB}} or  \code{\link[raster:plotRGB]{plot}}
#' @note This function is a wrapper for \code{\link[raster:plotRGB]{plotRGB}}
#' and  \code{\link[raster:plotRGB]{plot}}
#' from the raster package. The accuracy of the final plot depends on the
#' quality of the \code{*.png} file, the scale of \code{x} and the resolution
#' setup of the graphic device.
#' @export
#' @return No return value, a map is displayed.
#' @examples
#' library("raster")
#' r <- raster(system.file("external/test.grd", package = "raster"))
#' mf_raster(r)
mf_raster <- function(x, add = FALSE, ...) {
  if (!requireNamespace("raster", quietly = TRUE)) {
    stop(
      "'raster' package needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  if (!"package:raster" %in% search()) {
    attachNamespace("raster")
  }


  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  on.exit(par(op))

  if (add == FALSE) {
    bbx <- sf::st_as_sfc(sf::st_bbox(x))
    mf_base(x = bbx, col = NA, border = NA, add = FALSE)
  }

  if (is(x, "RasterBrick")) {
    ops <- list(...)
    ops$x <- x
    ops$add <- TRUE
    # Default opts
    ops$maxpixels <- ifelse(is.null(ops$maxpixels), raster::ncell(x),
      ops$maxpixels
    )
    ops$bgalpha <- ifelse(is.null(ops$bgalpha), 0, ops$bgalpha)
    ops$interpolate <- ifelse(is.null(ops$interpolate), TRUE, ops$interpolate)
    do.call(raster::plotRGB, ops)
  }
  if (is(x, "RasterLayer")) {
    ops <- list(...)
    ops$x <- x
    ops$add <- TRUE
    # Default opts
    ops$legend <- ifelse(is.null(ops$legend), FALSE, ops$legend)
    ops$axes <- ifelse(is.null(ops$axes), FALSE, ops$axes)
    ops$box <- ifelse(is.null(ops$box), FALSE, ops$box)
    ops$maxpixels <- ifelse(is.null(ops$maxpixels), raster::ncell(x),
      ops$maxpixels
    )
    ops$bgalpha <- ifelse(is.null(ops$bgalpha), 0, ops$bgalpha)
    ops$interpolate <- ifelse(is.null(ops$interpolate), FALSE, ops$interpolate)
    do.call(raster::plot, ops)
  }
}
