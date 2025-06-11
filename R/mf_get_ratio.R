#' @title Get map width and height values
#' @description This function is to be used to get width and height values
#' for maps created in reports (*.Rmd, *.qmd).\cr
#' It uses the width / height ratio of a spatial object bounding box to find a
#' matching ratio for the map.\cr
#' If width is specified, then height is deduced from the width / height ratio
#' of x, figure margins and title size.\cr
#' If height is specified, then width is
#' deduced from the width / height ratio of x, figure margins and title size.
#' @param x object of class \code{sf}, \code{sfc} or \code{SpatRaster}
#' @param expandBB fractional values to expand the bounding box with, in each
#' direction (bottom, left, top, right)
#' @param theme theme used for the map
#' @param width width of the figure (inches), use only one of width or height
#' @param height height of the figure (inches), use only one of width or height
#' @importFrom sf st_bbox st_as_sfc st_geometry st_is_longlat st_crs
#' @return Width and height are returned in inches.
#' @export
#'
#' @examples
#' mtq <- mf_get_mtq()
#' mf_get_ratio(x = mtq, width = 5)
mf_get_ratio <- function(x,
                         width, height,
                         expandBB = rep(0, 4),
                         theme = mf_theme()) {
  # input test
  test_ratio_input(x)
  bb <- x_to_bb(x = x, expandBB = expandBB)
  mar <- get_mar(theme)
  bb_to_ratio(bb = bb, mar = mar, width = width, height = height)
}


test_ratio_input <- function(x) {
  # input test
  if (!inherits(x, c("bbox", "SpatVector", "SpatRaster", "sf", "sfc", "sfg"))) {
    stop(
      paste0(
        "x should be an object of class sf, sfc, sfg, bbox, ",
        "SpatRaster or SpatVector"
      ),
      call. = FALSE
    )
  }

  if (inherits(x, c("SpatRaster", "SpatVector"))) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop(
        paste0(
          "'terra' package is needed for this function to work. ",
          "Please install it."
        ),
        call. = FALSE
      )
    }
  }
}



x_to_bb <- function(x, expandBB) {
  if (inherits(x, c("SpatRaster", "SpatVector"))) {
    ras_proj <- terra::crs(x)
    x <- st_as_sfc(st_bbox(terra::ext(x)[c(1, 3, 2, 4)]))
    st_crs(x) <- ras_proj
    expandBB <- expandBB + c(rep(-.04, 4))
  } else {
    if (par("xaxs") == "r") {
      expandBB <- expandBB / (1 + 0.08)
    }
  }

  if (isTRUE(st_is_longlat(st_crs(x)))) {
    x <- st_as_sfc(st_bbox(x))
    lat_ts <- mean(sf::st_bbox(x)[c(2, 4)]) # latitude of true scale
    x <- st_transform(x = x, paste0("+proj=eqc +lat_ts=", lat_ts))
  }

  bb <- st_bbox(x)
  # expandBB mgmt
  w <- bb[3] - bb[1]
  h <- bb[4] - bb[2]
  bb <- bb + (expandBB[c(2, 1, 4, 3)] * c(-w, -h, w, h))

  return(bb)
}

get_mar <- function(theme) {
  if (missing(theme)) {
    mar <- getOption("mapsf.mar")
  } else {
    old_theme <- mf_theme()
    mf_theme(theme)
    mar <- getOption("mapsf.mar")
    mf_theme(old_theme)
  }
  mar
}



bb_to_ratio <- function(bb, mar, width, height) {
  if (!missing(width) && !missing(height)) {
    return(c(width, height))
  }
  iw <- bb[3] - bb[1]
  ih <- bb[4] - bb[2]
  if (missing(width) && missing(height)) {
    width <- 7
  }
  if (!missing(width) && width > 50) {
    message(paste0(
      "It is unlikely that you really want to produce a figure",
      " with more than 50 inches of width.", " The width has been",
      " set to 7 inches."
    ))
    width <- 7
  }
  if (missing(height)) {
    wh <- iw / ih
    widthmar <- width - (0.2 * (mar[2] + mar[4]))
    height <- (widthmar / wh) + (0.2 * (mar[1] + mar[3]))
  } else {
    hw <- ih / iw
    heightmar <- height - (0.2 * (mar[1] + mar[3]))
    width <- (heightmar / hw) + (0.2 * (mar[2] + mar[4]))
  }
  return(unname(round(c(width, height), 3)))
}
