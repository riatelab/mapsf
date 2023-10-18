
#' @title Plot a raster
#' @description Plot a raster object (SpatRaster from terra).
#' @name mf_raster
#' @param x a SpatRaster
#' @param expandBB fractional values to expand the bounding box with, in each
#' direction (bottom, left, top, right)
#' @param add whether to add the layer to an existing plot (TRUE) or
#' not (FALSE)
#' @param leg_horiz display the legend horizontally
#' @eval my_params(c(
#' "pal",
#' "alpha",
#' 'leg_pos',
#' 'leg_title',
#' 'leg_title_cex',
#' 'leg_val_cex',
#' 'leg_val_rnd',
#' 'leg_no_data',
#' 'leg_frame_border',
#' 'leg_adj',
#' 'leg_fg',
#' 'leg_bg',
#' 'leg_size',
#' 'leg_frame'))
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
mf_raster <- function(x,
                      pal,
                      expandBB = rep(0, 4),
                      alpha = 1,
                      leg_pos = "right",
                      leg_title = names(x),
                      leg_title_cex = .8,
                      leg_val_cex = .6,
                      leg_val_rnd = 2,
                      leg_no_data = "No data",
                      leg_frame = FALSE,
                      leg_frame_border = getOption("mapsf.fg"),
                      leg_horiz = FALSE,
                      leg_adj = c(0, 0),
                      leg_fg = getOption("mapsf.fg"),
                      leg_bg = getOption("mapsf.bg"),
                      leg_size = 1,
                      add = FALSE,
                      ...) {
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
  mcell <- terra::ncell(x)
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
    if (missing(pal)) {
      pal <- "custom"
    }
    pal <- get_the_raster_pal(pal = pal, nbreaks = 255, alpha = alpha)
    ops$col <- pal[-1]
    ops$smooth <- ifelse(is.null(ops$smooth), FALSE, ops$smooth)
    ops$legend <- FALSE
    ops$axes <- FALSE
    ops$box <- FALSE
    do.call(terra::plot, ops)
    # For the legend
    val <- terra::values(x)
    v <- mf_get_breaks(x = val, nbreaks = 4, breaks = "pretty")
    vmin <- min(val, na.rm = TRUE)
    vmax <- max(val, na.rm = TRUE)
    vv <- c(vmin, v[v > vmin & v < vmax], vmax)
    leg(
      type = "cont", box_cex = c(1.5, 2),
      val = vv, horiz = leg_horiz,
      pos = leg_pos, pal = pal,
      title = leg_title, title_cex = leg_title_cex,
      val_cex = leg_val_cex, val_rnd = leg_val_rnd,
      frame = leg_frame, bg = leg_bg, fg = leg_fg,
      frame_border = leg_frame_border, adj = leg_adj,
      size = leg_size
    )
  }
}

get_the_raster_pal <- function(pal, nbreaks, alpha = 1) {
  if (pal == "custom") {
    return(rev(grDevices::terrain.colors(255)))
  }
  if (length(pal) == 1) {
    if (pal %in% hcl.pals()) {
      cols <- hcl.colors(n = nbreaks, palette = pal, alpha = alpha, rev = TRUE)
    } else {
      stop("This is not a palette name", call. = FALSE)
    }
  } else {
    cols <- colorRampPalette(pal, alpha = TRUE)(nbreaks)
  }
  return(cols)
}
