#' @title Export a map
#' @name mf_export
#' @description Export a map with the extent of a spatial object.\cr
#' The map is exported in PNG or SVG format.\cr
#' If only one of \code{width} or \code{height} is set, \code{mf_export} uses
#' the width/height ratio of \code{x} bounding box to find a matching ratio for
#' the export.\cr
#' Always use \code{add = TRUE} in \code{mf_map} calls following an
#' \code{mf_export} call.\cr
#' Use \code{dev.off} to finish the export (see Examples).
#' @param x object of class \code{sf}, \code{sfc} or \code{SpatRaster}
#' @param expandBB fractional values to expand the bounding box with, in each
#' direction (bottom, left, top, right)
#' @param theme apply a theme (deprecated)
#' @param filename path to the exported file. If the file extention is ".png" a
#' png graphic device is opened, if the file extension is ".svg" a svg graphic
#' device is opened.
#' @param export deprecated
#' @param width width of the figure (pixels for png, inches for svg)
#' @param height height of the figure (pixels for png, inches for svg)
#' @param res resolution (for png)
#' @param ... further parameters for png or svg export
#' @export
#' @importFrom grDevices png svg
#' @importFrom sf st_bbox st_as_sfc st_geometry st_is_longlat st_crs
#' @return No return value, a map file is initiated (in PNG or SVG format).
#' @examples
#' mtq <- mf_get_mtq()
#' (filename <- tempfile(fileext = ".png"))
#' mf_export(mtq, filename = filename)
#' mf_map(mtq, add = TRUE)
#' dev.off()
mf_export <- function(x,
                      filename = "map.png",
                      width,
                      height,
                      res = 96,
                      ...,
                      expandBB = rep(0, 4),
                      theme,
                      export = "png") {
  # deprecated args mgmt
  if (!missing(theme)) {
    warning(
      paste0(
        "'theme' is deprecated.\n",
        "In the next version of mapsf the current theme ",
        "will be applied to the export."
      ),
      call. = FALSE
    )
    mf_theme(theme)
  }
  if (!missing(export)) {
    message('"export" is deprecated.')
  }

  # input test
  if(!inherits(x, c("bbox", "SpatVector", "SpatRaster", "sf", "sfc", "sfg"))){
    stop(paste0("x should be an object of class sf, sfc, sfg, bbox, ",
                "SpatRaster or SpatVector"),
         call. = FALSE)
  }

  # file format
  nc <- nchar(filename)
  ext <- substr(filename, nc - 3, nc)
  if (ext %in% c(".png", ".svg")) {
    export <- substr(ext, 2, 4)
  } else {
    stop('The filename extension should be ".png" or ".svg".', call. = FALSE)
  }


  if (inherits(x, "SpatRaster")) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop(paste0("'terra' package is needed for this function to work. ",
                  "Please install it."),
        call. = FALSE
      )
    }
    proj <- terra::crs(x)
    bb <- terra::ext(x)[c(1, 3, 2, 4)]
    y <- st_as_sfc(st_bbox(bb))
    st_crs(y) <- proj
    mf_export(
      x = y, filename = filename, width = width, height = height, res = res,
      expandBB = c(rep(-.04, 4)) + expandBB, ...
    )
    return(invisible(x))
  }

  mar <- getOption("mapsf.mar")
  bgmap <- getOption("mapsf.bg")

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


  if (export == "png") {
    if (!missing(width) & !missing(height)) {
      fd <- c(width, height)
    } else {
      if (missing(width) & missing(height)) {
        width <- 600
      }
      fd <- get_ratio(
        x = bb,
        width = width,
        height = height,
        mar = mar,
        res = res,
        format = "png"
      )
    }
    if (isTRUE(capabilities("cairo"))) {
      png(filename, width = fd[1], height = fd[2], res = res, type = "cairo-png", ...)
    } else {
      png(filename, width = fd[1], height = fd[2], res = res, ...)
    }
  }
  if (export == "svg") {
    if (!missing(width) & !missing(height)) {
      fd <- c(width, height)
    } else {
      if (missing(height) & missing(width)) {
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
      fd <- get_ratio(
        x = bb,
        width = width,
        height = height,
        mar = mar, res = 96, format = "svg"
      ) / 96
    }
    svg(filename = filename, width = fd[1], height = fd[2], ...)
  }

  # margins mgmt
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))
  # plot with bg and margins
  plot(y, col = NA, border = NA, expandBB = expandBB)
  pux <- par("usr")
  rect(pux[1], pux[3], pux[2], pux[4], border = NA, col = bgmap)

  return(invisible(NULL))
}






get_ratio <- function(x, width, height, mar, res, format) {
  if(isTRUE(sf::st_is_longlat(x))){
    x <- st_as_sfc(x)
    lat_ts <- mean(sf::st_bbox(x)[c(2,4)]) # latitude of true scale
    x <- st_transform(x = x,paste0("+proj=eqc +lat_ts=", lat_ts))
    x <- st_bbox(x)
  }

  iw <- x[3] - x[1]
  ih <- x[4] - x[2]
  if (missing(height)) {
    if (format == "svg") {
      width <- width * 96
    }
    wh <- iw / ih
    widthmar <- width - (0.2 * (mar[2] + mar[4]) * res)
    height <- (widthmar / wh) + (0.2 * (mar[1] + mar[3]) * res)
  } else {
    if (format == "svg") {
      height <- height * 96
    }
    hw <- ih / iw
    heightmar <- height - (0.2 * (mar[1] + mar[3]) * res)
    width <- (heightmar / hw) + (0.2 * (mar[2] + mar[4]) * res)
  }
  return(unname(floor(c(width, height))))
}
