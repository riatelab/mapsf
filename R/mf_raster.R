#' @title Plot a raster
#' @description Plot a raster object (SpatRaster from terra).
#' @name mf_raster
#' @param x a SpatRaster
#' @param type type of raster map, one of "continuous", "classes", or
#' "interval". Default type for a numeric and categorial raster are
#' "continuous" and "classes" respectively.
#' @param expandBB fractional values to expand the bounding box with, in each
#' direction (bottom, left, top, right)
#' @param add whether to add the layer to an existing plot (TRUE) or
#' not (FALSE)
#' @param leg_horiz display the legend horizontally
#' @param breaks either a numeric vector with the actual breaks
#' (for type = "continuous" and type = "interval"), or a
#' classification method name (for type = "interval" only;
#' see \link{mf_get_breaks} for classification methods)
#' @eval my_params(c(
#' "nbreaks",
#' "val_order",
#' "pal",
#' "alpha",
#' 'rev',
#' 'leg_pos',
#' 'leg_title',
#' 'leg_title_cex',
#' 'leg_val_cex',
#' 'leg_val_rnd',
#' 'leg_box_border',
#' 'leg_box_cex',
#' 'leg_frame_border',
#' 'leg_adj',
#' 'leg_fg',
#' 'leg_bg',
#' 'leg_size',
#' 'leg_frame'))
#' @param ... bgalpha, smooth, maxcell or other arguments passed to be
#' passed to
#' \code{\link[terra:plotRGB]{plotRGB}} or \code{\link[terra:plot]{plot}}
#' @export
#' @return x is (invisibly) returned.
#' @examples
#' if (require("terra")) {
#'   # multi band
#'   logo <- rast(system.file("ex/logo.tif", package = "terra"))
#'   mf_raster(logo)
#'
#'   # one band
#'   elev <- rast(system.file("ex/elev.tif", package = "terra"))
#'
#'   ## continuous
#'   mf_raster(elev)
#'   mf_raster(elev,
#'     pal = "Burg", expandBB = c(.2, 0, 0, 0),
#'     leg_pos = "bottom", leg_horiz = TRUE
#'   )
#'
#'   ## continuous with colors and breaks
#'   mf_raster(elev,
#'     type = "continuous",
#'     breaks = c(141, 400, 547),
#'     pal = c("darkseagreen1", "black", "red")
#'   )
#'
#'   ## interval
#'   mf_raster(elev,
#'     type = "interval",
#'     nbreaks = 5, breaks = "equal", pal = "Teal"
#'   )
#'
#'   ## classes
#'   elev2 <- classify(elev, c(140, 400, 450, 549))
#'   lev_evel <- data.frame(ID = 0:2, elevation = c("Low", "High", "Super High"))
#'   levels(elev2) <- lev_evel
#'   mf_raster(elev2)
#'   mf_raster(elev2,
#'     pal = c("salmon4", "olivedrab", "yellow3"),
#'     val_order = c("Super High", "High", "Low")
#'   )
#' }
mf_raster <- function(x,
                      type,
                      nbreaks,
                      breaks = "equal",
                      val_order,
                      pal,
                      expandBB = rep(0, 4),
                      alpha = 1,
                      rev = FALSE,
                      leg_pos = "right",
                      leg_title = names(x),
                      leg_title_cex = .8,
                      leg_val_cex = .6,
                      leg_val_rnd = 1,
                      leg_frame = FALSE,
                      leg_frame_border = getOption("mapsf.fg"),
                      leg_horiz = FALSE,
                      leg_adj = c(0, 0),
                      leg_box_border = "#333333",
                      leg_box_cex = c(1, 1),
                      leg_fg = getOption("mapsf.fg"),
                      leg_bg = getOption("mapsf.bg"),
                      leg_size = 1,
                      add = FALSE,
                      ...) {
  # test for terra
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop(paste0(
      "'terra' package is needed for this function to work.",
      "Please install it."
    ), call. = FALSE)
  }

  # input test
  if (!inherits(x, "SpatRaster")) {
    stop(paste0("x should be a SpatRaster."), call. = FALSE)
  }

  # par
  op <- par(xpd = TRUE, no.readonly = TRUE)
  on.exit(par(op))

  # catch arguments
  ops <- list(...)
  ops$x <- x
  ops$add <- TRUE
  max_cell <- is.null(ops$maxcell) | terra::ncell(x) >= 1e6
  ops$maxcell <- ifelse(max_cell, 1e6, ops$maxcell)
  ops$bgalpha <- ifelse(is.null(ops$bgalpha), 0, ops$bgalpha)
  ops$legend <- ifelse(is.null(ops$legend), FALSE, ops$legend)
  ops$axes <- FALSE
  ops$box <- FALSE
  ops$mar <- NA

  # Multiband Raster
  if (terra::nlyr(x) >= 2) {
    mf_raster_multiband(ops, expandBB, add)
  }

  # One band raster
  if (terra::nlyr(x) == 1) {
    # set the type - default to continuous for numeric raster
    if (missing(type)) {
      ops$type <- ifelse(is.null(terra::cats(x)[[1]]), "continuous", "classes")
    } else {
      if (!type %in% c("continuous", "interval", "classes")) {
        stop("'type' should be one of 'continuous', 'interval' or 'classes'.",
          call. = FALSE
        )
      }
      ops$type <- type
    }

    # common args
    ops_leg <- mget(ls(pattern = "leg_"))
    ops$smooth <- ifelse(is.null(ops$smooth), FALSE, ops$smooth)

    if (ops$type == "interval") {
      mf_raster_interval(
        ops, ops_leg, pal, breaks, nbreaks, alpha, rev, add,
        expandBB
      )
    }

    if (ops$type == "continuous") {
      mf_raster_continuous(ops, ops_leg, breaks, pal, expandBB, add, alpha, rev)
    }

    if (ops$type == "classes") {
      mf_raster_classes(ops, ops_leg, pal, val_order, expandBB, add, alpha, rev)
    }
  }

  return(invisible(x))
}
