#' @title Plot a scale bar
#' @description Plot a scale bar.
#' @name mf_scale
#' @param col color of the scale bar (line and text)
#' @param size size of the scale bar in scale units (\code{scale_units},
#' default to km). If size is not
#' set, an automatic size is used (1/10 of the map width).
#' @param lwd line width of the scale bar
#' @param cex size of the scale bar text
#' @param pos position. It can be one of 'bottomright', 'bottomleft',
#' 'interactive' or a vector of two coordinates in map units (c(x, y)).
#' @param crs_units units used in the CRS of the currently plotted layer.
#' Possible values are "m" and "ft" (see Details).
#' @param scale_units units used for the scale bar. Can be "mi" for miles,
#' "ft" for feet, "m" for meters, or "km" for kilometers (default).
#' @param x object of class crs, sf or sfc. If set, the CRS of x will be used
#' instead of \code{crs_units} to define CRS units.
#' @details Most CRS use the meter as unit. Some US CRS use feet or US survey
#' feet. If unsure of the unit used in the CRS you can use the x argument of the
#' function.
#' Alternatively, you can use
#' \code{sf::st_crs(zz, parameters = TRUE)$units_gdal} to see which units
#' are used in the \code{zz} layer.
#'
#' This scale bar does not work on unprojected (long/lat) maps.
#'
#' @return No return value, a scale bar is displayed.
#' @export
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_scale()
#'
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"))[1, ]
#'
#' nc_foot <- st_transform(nc, 2264) # NC state plane, US foot
#' mf_map(nc_foot)
#' mf_scale(size = 5, crs_units = "ft", scale_units = "mi")
#' mf_map(nc_foot)
#' mf_scale(size = 5, x = nc_foot, scale_units = "mi")
#'
#' nc_meter <- st_transform(nc, 32119) # NC state plane, m
#' mf_map(nc_meter)
#' mf_scale(size = 5, crs_units = "m", scale_units = "mi")
#' mf_scale(size = 5, crs_units = "m", scale_units = "km", pos = "bottomleft")
mf_scale <- function(size,
                     pos = "bottomright",
                     lwd = 1.5,
                     cex = 0.6,
                     col,
                     crs_units = "m",
                     scale_units = "km",
                     x) {
  test_cur_plot()
  # default color
  if (missing(col)) {
    col <- getOption("mapsf.fg")
  }
  # get the current plot dimensions
  pu <- par("usr")
  inset <- xinch(par("csi")) / 4

  if (!missing(x)) {
    uu <- sf::st_crs(x)$ud_unit
    u_m <- structure(1,
      units = structure(
        list(
          numerator = "m",
          denominator = character(0)
        ),
        class = "symbolic_units"
      ),
      class = "units"
    )
    u_f <- structure(1,
      units = structure(
        list(
          numerator = "foot",
          denominator = character(0)
        ),
        class = "symbolic_units"
      ),
      class = "units"
    )
    u_sf <- structure(1,
      units = structure(
        list(
          numerator = "US_survey_foot",
          denominator = character(0)
        ),
        class = "symbolic_units"
      ),
      class = "units"
    )
    u_d <- structure(1,
      units = structure(
        list(
          numerator = "\u00b0",
          denominator = character(0)
        ),
        class = "symbolic_units"
      ),
      class = "units"
    )

    if (identical(uu, u_m)) {
      crs_units <- "m"
    } else {
      if (identical(uu, u_f) || identical(uu, u_sf)) {
        crs_units <- "ft"
      } else {
        if (identical(uu, u_d)) {
          message("The scale bar does not work on unprojected (long/lat) maps.")
        } else {
          message("The scale bar does not work on maps without documented CRS.")
        }
        return(invisible(NULL))
      }
    }
  }

  if (!crs_units %in% c("m", "ft")) {
    stop("crs_units must be 'm' or 'ft'.")
  }
  if (!scale_units %in% c("km", "m", "ft", "mi")) {
    stop("scale_units must be 'km', 'm', 'ft' or 'mi'.")
  }



  # default scale
  if (missing(size)) {
    size <- diff(pu[1:2]) / 10
    size <- unit_conversion(
      size = size,
      unit_in = crs_units,
      unit_out = scale_units
    )
    size_text <- signif(size, digits = 0)
    size <- unit_conversion(
      size = size_text,
      unit_in = scale_units,
      unit_out = crs_units
    )
  } else {
    size_text <- as.character(size)
    size <- unit_conversion(size,
      unit_in = scale_units,
      unit_out = crs_units
    )
  }

  # label
  labelscale <- paste0(size_text, " ", scale_units)

  # xy pos
  xscale <- pu[2] - inset - size
  yscale <- pu[3] + inset

  if (!missing(pos)) {
    if (is.numeric(pos) && length(pos) == 2) {
      xscale <- pos[1]
      yscale <- pos[2]
    } else {
      if (pos == "interactive") {
        isc <- interleg(txt = c("scale bar", "Scale bar"))
        xscale <- isc[1]
        yscale <- isc[2]
      }
      if (pos == "bottomleft") {
        xscale <- pu[1] + inset
        yscale <- pu[3] + inset
      }
    }
  }

  # plot the scale bar
  segments(
    x0 = xscale,
    y0 = yscale,
    x1 = xscale + size,
    y1 = yscale,
    lwd = lwd,
    col = col,
    xpd = TRUE
  )
  # plot the scale bar label
  text(
    xscale + (size / 2),
    yscale,
    adj = c(0.5, -.5),
    labels = labelscale,
    cex = cex,
    col = col,
    xpd = TRUE
  )
}


#' Convert units
#' @param size a size
#' @param unit_in input unit
#' @param unit_out output unit
#' @noRd
unit_conversion <- function(size, unit_in, unit_out) {
  if (unit_out == "m") {
    if (unit_in == "ft") size <- size / 3.28084
    if (unit_in == "km") size <- size * 1000
    if (unit_in == "mi") size <- size * 1609.34
  }
  if (unit_out == "km") {
    if (unit_in == "m") size <- size / 1000
    if (unit_in == "ft") size <- size / 3280.84
    if (unit_in == "mi") size <- size * 1.60934
  }
  if (unit_out == "mi") {
    if (unit_in == "m") size <- size / 1609.34
    if (unit_in == "ft") size <- size / 5280
    if (unit_in == "km") size <- size / 1.60934
  }
  if (unit_out == "ft") {
    if (unit_in == "m") size <- size * 3.28084
    if (unit_in == "km") size <- size * 3280.84
    if (unit_in == "mi") size <- size * 5280
  }

  return(size)
}
