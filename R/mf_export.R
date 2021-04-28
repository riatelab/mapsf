#' @title Export a map
#' @name mf_export
#' @description Export a map with the extent of a spatial object.
#' The map is exported in PNG or SVG format. If
#' only one of \code{width} or \code{height} is set, \code{mf_export} uses the
#' width/height ratio of \code{x} bounding box to find a matching ratio for
#' the export.
#' @param x object of class \code{sf}, \code{sfc} or \code{Raster}
#' @param expandBB fractional values to expand the bounding box with, in each
#' direction (bottom, left, top, right)
#' @param theme apply a theme from \code{mf_theme}
#' @param filename path to the exported file
#' @param export if set to "png" or "svg" a png or svg plot device is opened
#' @param width width of the figure (pixels for png, inches for svg)
#' @param height height of the figure (pixels for png, inches for svg)
#' @param res resolution (for png)
#' @param ... further parameters for png or svg export
#' @export
#' @importFrom grDevices png svg
#' @importFrom methods is
#' @importFrom sf st_bbox st_as_sfc st_geometry
#' @return No return value, a map is initiated.
#' @examples
#' mtq <- mf_get_mtq()
#' (filename <- tempfile(fileext = ".png"))
#' mf_export(mtq, filename = filename)
#' mf_map(mtq, add = TRUE)
#' dev.off()
mf_export <- function(x,
                      export = "png",
                      filename = paste0("map.", export),
                      width,
                      height,
                      res = 96, ...,
                      expandBB = rep(0, 4),
                      theme = "default") {
  if (!missing(theme)) {
    mf_theme(theme)
  }
  mar <- .gmapsf$args$mar
  bgmap <- .gmapsf$args$bg

  if (is(x, "Raster")) {
    bb <- st_bbox(x)
    xd <- diff(bb[c(1, 3)]) * 0.04
    yd <- diff(bb[c(2, 4)]) * 0.04
    nbb <- bb + c(xd, yd, -xd, -yd)
    x <- st_as_sfc(st_bbox(nbb), crs = st_crs(x))
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
        mar = mar, res = res, format = "svg"
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
  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
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
