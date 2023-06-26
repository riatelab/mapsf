#' @title Initialize a map with a specific extent
#' @name mf_init
#' @description Plot an invisible layer with the extent of a spatial object.\cr
#' Always use \code{add = TRUE} in \code{mf_map} calls following an
#' \code{mf_init} call.
#' @param x object of class \code{sf}, \code{sfc} or \code{SpatRaster}
#' @param expandBB fractional values to expand the bounding box with, in each
#' direction (bottom, left, top, right)
#' @param theme apply a theme (deprecated)
#' @export
#' @importFrom sf st_bbox st_as_sfc st_geometry `st_crs<-`
#' @return No return value, a map is initiated.
#' @examples
#' mtq <- mf_get_mtq()
#' target <- mtq[30, ]
#' mf_init(target)
#' mf_map(mtq, add = TRUE)
mf_init <- function(x, expandBB = rep(0, 4), theme) {
  if (!missing(theme)) {
    warning(
      paste0(
        "'theme' is deprecated.\n",
        "In the next version of mapsf the current theme ",
        "will be applied."
      ),
      call. = FALSE
    )
    mf_theme(theme)
  }
  bgmap <- getOption("mapsf.bg")

  if (inherits(x, "SpatRaster")) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop(
        paste0(
          "'terra' package is needed for this function to work. ",
          "Please install it."
        ),
        call. = FALSE
      )
    }

    proj <- terra::crs(x)
    bb <- terra::ext(x)[c(1, 3, 2, 4)]
    y <- st_as_sfc(st_bbox(bb))
    st_crs(y) <- proj
    mf_init(y, expandBB = c(rep(-.04, 4)) + expandBB)
    return(invisible(x))
  }

  # transform to bbox
  bb <- st_bbox(x)
  y <- st_as_sfc(bb, crs = st_crs(x))

  if (par("xaxs") == "r") {
    expandBB <- expandBB / (1 + 0.08)
  }

  # margins mgmt
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))

  # plot with bg and margins
  plot(y, col = NA, border = NA, expandBB = expandBB)
  pux <- par("usr")
  rect(pux[1], pux[3], pux[2], pux[4], border = NA, col = bgmap)

  return(invisible(x))
}
