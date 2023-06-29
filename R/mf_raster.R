#' @title Plot a raster
#' @description Plot a raster object (SpatRaster from terra).
#' @name mf_raster
#' @param x a SpatRaster
#' @param expandBB fractional values to expand the bounding box with, in each
#' direction (bottom, left, top, right)
#' @param add whether to add the layer to an existing plot (TRUE) or
#' not (FALSE).
#' @param ... bgalpha, smooth, maxcell or other arguments passed to be
#' passed to
#' \code{\link[terra:plotRGB]{plotRGB}} or  \code{\link[terra:plot]{plot}}
#' @export
#' @return No return value, a map is displayed.
#' @examples
#' if (require("terra")) {
#'   r <- rast(system.file("ex/elev.tif", package = "terra"))
#'   mf_raster(r)
#' }
mf_raster <- function(x, expandBB = rep(0, 4), add = FALSE, ...) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop(
      "'terra' package is needed for this function to work. Please install it.",
      call. = FALSE
    )
  }

  # input test
  if (!inherits(x, "SpatRaster")) {
    stop(paste0("x should be a SpatRaster."),
      call. = FALSE
    )
  }


  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))

  if (add == FALSE) {
    mf_init(x, expandBB = expandBB)
  }
  # maxcell mgmt
  dx <- dim(x)
  mcell <- dx[1] * dx[2]
  if (mcell >= 1e6) {
    mcell <- 1e6
  }

  ops <- list(...)
  ops$x <- x
  ops$add <- TRUE
  ops$maxcell <- ifelse(is.null(ops$maxcell), mcell, ops$maxcell)
  ops$bgalpha <- ifelse(is.null(ops$bgalpha), 0, ops$bgalpha)
  if (terra::nlyr(x) >= 2) {
    ops$smooth <- ifelse(is.null(ops$smooth), TRUE, ops$smooth)
    do.call(terra::plotRGB, ops)
  }
  if (terra::nlyr(x) == 1) {
    ops$smooth <- ifelse(is.null(ops$smooth), FALSE, ops$smooth)
    ops$legend <- ifelse(is.null(ops$legend), FALSE, ops$legend)
    ops$axes <- ifelse(is.null(ops$axes), FALSE, ops$axes)
    ops$box <- ifelse(is.null(ops$box), FALSE, ops$box)
    do.call(terra::plot, ops)
  }
}
