#' @title Defunct - Initialize a map with a specific extent
#' @name mf_init
#' @description
#' This function is defunct.
#' It is possible to use `mf_map` instead
#' (`mf_map(x, type = "base", col = NA, border = NA)`.
#' It is also possible to use the `extent` argument of [mf_map].
#' @export
#' @keywords internal
#' @return No return value, a map is initiated.
#' @examples
#' mtq <- mf_get_mtq()
#' target <- mtq[30, ]
#' mf_map(target, type = "base", col = NA, border = NA)
#' mf_map(mtq, add = TRUE)
#' # or
#' mf_map(mtq, extent = target)
mf_init <- function(...){
  .Defunct(
    msg = "'mf_init()' is defunct.\nUse 'mf_map()' instead."
  )
}


mf_ini <- function(x, expandBB = rep(0, 4), extent = x, bgc) {
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
    mf_ini(y,
      expandBB = c(rep(-.0399, 4)) + expandBB,
      extent = extent, bgc = bgc
    )
    return(invisible(x))
  }

  # transform to sfc bbox
  y <- st_as_sfc(st_bbox(x), crs = st_crs(x))

  if (par("xaxs") == "r") {
    expandBB <- expandBB / (1 + 0.08)
  }

  # margins mgmt
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))

  if (missing(bgc)) {
    bgc <- getOption("mapsf.background")
  }

  if (st_crs(x) != st_crs(extent)) {
    warning(
      paste0(
        "'x' and 'extent' must use the same CRS.",
        " 'extent' is ignored."
      ),
      call. = FALSE
    )
    extent <- y
  }
  # plot with bg and margins
  plot(y, col = NA, border = NA, expandBB = expandBB, extent = extent)

  recordGraphics(
    {
      pux <- par("usr")
      rect(pux[1],
        pux[3],
        pux[2],
        pux[4],
        border = NA,
        col = bgc
      )
    },
    list = list(bgc = bgc),
    env = getNamespace("mapsf")
  )

  f <- getOption("mapsf.frame")
  if (f %in% c("map", "figure")) {
    mf_frame(
      extent = f,
      col = getOption("mapsf.highlight"),
      lwd = getOption("mapsf.frame_lwd"),
      lty = getOption("mapsf.frame_lty")
    )
  }
  return(invisible(x))
}
