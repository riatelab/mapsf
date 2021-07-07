#' @title Initialize a map with a specific extent
#' @name mf_init
#' @description Plot an invisible layer with the extent of a spatial object.
#' @param x object of class \code{sf}, \code{sfc} or \code{Raster}
#' @param expandBB fractional values to expand the bounding box with, in each
#' direction (bottom, left, top, right)
#' @param theme apply a theme from \code{mf_theme}
#' @export
#' @importFrom methods is
#' @importFrom sf st_bbox st_as_sfc st_geometry
#' @return No return value, a map is initiated.
#' @examples
#' mtq <- mf_get_mtq()
#' target <- mtq[30, ]
#' mf_init(target)
#' mf_map(mtq, add = TRUE)
mf_init <- function(x,
                    expandBB = rep(0, 4),
                    theme) {
  if (!missing(theme)) {
    mf_theme(theme)
  }
  mar <- .gmapsf$args$mar
  bgmap <- .gmapsf$args$bg

  if (is(x, "SpatRaster")) {
    bb <- terra::ext(x)[c(1,3,2,4)]
    xd <- diff(bb[c(1, 3)])
    yd <- diff(bb[c(2, 4)]) * 0.04
    nbb <- bb + c(xd, yd, -xd, -yd)
    x <- st_as_sfc(st_bbox(nbb), crs = terra::crs(x))
  }

  # transform to bbox
  bb <- st_bbox(x)
  y <- st_as_sfc(bb)

  if (par("xaxs") == "r") {
    expandBB <- expandBB / (1 + 0.08)
  }
  # expandBB mgmt
  extra <- expandBB[c(2, 1, 4, 3)]
  w <- bb[3] - bb[1]
  h <- bb[4] - bb[2]
  bb <- bb + (extra * c(-w, -h, w, h))

  # margins mgmt
  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  on.exit(par(op))

  # plot with bg and margins
  plot(y, col = NA, border = NA, expandBB = expandBB)
  pux <- par("usr")
  rect(pux[1], pux[3], pux[2], pux[4], border = NA, col = bgmap)

  return(invisible(x))
}
