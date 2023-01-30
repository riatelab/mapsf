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
#' @param res resolution
#' @importFrom methods is
#' @importFrom sf st_bbox st_as_sfc st_geometry st_is_longlat st_crs
#' @return Width and height are returned in inches.
#' @export
#'
#' @examples
#' mtq <- mf_get_mtq()
#' mf_get_ratio(x = mtq, width = 5)
mf_get_ratio <- function(x,
                         width, height,
                         res = 96,
                         expandBB = rep(0, 4),
                         theme = mf_theme()) {
  if (is(x, "SpatRaster")) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop(
        "'terra' package is needed for this function to work. Please install it.",
        call. = FALSE
      )
    }
    proj <- terra::crs(x)
    bb <- terra::ext(x)[c(1, 3, 2, 4)]
    y <- st_as_sfc(st_bbox(bb))
    st_crs(y) <- proj
    expandBB <- c(rep(-.04, 4))
  }

  if (isTRUE(st_is_longlat(st_crs(x)))) {
    message(paste0(
      "Exports using unprojected objects may produce figures ",
      "with inaccurate height/width ratio. ",
      "You may want to check 'x' CRS. "
    ))
  }

  if(missing(theme)){
    mar <- getOption("mapsf.mar")
  }else{
    old_theme <- mf_theme()
    mf_theme(theme)
    mar <- getOption("mapsf.mar")
    mf_theme(old_theme)
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

  # get the ratio
  iw <- bb[3] - bb[1]
  ih <- bb[4] - bb[2]

  if (missing(width) && missing(height)) {
    width <- 7
  }

  if (missing(height)) {
    width <- width * 96
    wh <- iw / ih
    widthmar <- width - (0.2 * (mar[2] + mar[4]) * res)
    height <- (widthmar / wh) + (0.2 * (mar[1] + mar[3]) * res)
  } else {
    height <- height * 96
    hw <- ih / iw
    heightmar <- height - (0.2 * (mar[1] + mar[3]) * res)
    width <- (heightmar / hw) + (0.2 * (mar[2] + mar[4]) * res)
  }

  return(unname(round(c(width, height) / 96, 2)))
}
