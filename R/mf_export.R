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
#' @param theme apply a theme
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
#' @importFrom methods is
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
  if (!missing(theme)) {
    mf_theme(theme)
  }
  mar <- getOption("mapsf.mar")
  bgmap <- getOption("mapsf.bg")
  if (!missing(export)) {
    message('"export" is deprecated.')
  }
  nc <- nchar(filename)
  ext <- substr(filename, nc - 3, nc)
  if (ext == ".png") {
    export <- "png"
  }
  if (ext == ".svg") {
    export <- "svg"
  }
  if (!ext %in% c(".png", ".svg")) {
    stop('The filename extension should be ".png" or ".svg".', call. = FALSE)
  }


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
    mf_export(
      x = y, filename = filename, width = width, height = height, res = res,
      expandBB = c(rep(-.04, 4)) + expandBB, theme = theme, ...
    )
    return(invisible(x))
  }

  if (isTRUE(st_is_longlat(st_crs(x)))) {
    message(paste0(
      "Exports using unprojected objects may produce figures ",
      "with inaccurate height/width ratio. ",
      "You may want to check 'x' CRS. "
    ))
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


  if (export == "png") {
    if (!missing(width) & !missing(height)) {
      fd <- c(width, height)
    } else {
      if (missing(width) & missing(height)) {
        width <- 600
      }
      fd <- get_ratio(
        x = bb, width = width, height = height,
        mar = mar, res = res, format = "png"
      )
    }
    png(filename, width = fd[1], height = fd[2], res = res, ...)
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
  if (!missing(theme)) {
    mf_theme(theme)
  } else {
    mf_theme(mf_theme())
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







get_ratio <- function(x, width, height, mar, res, format) {
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
