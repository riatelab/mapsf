#' @title Plot graticules
#' @description Display graticules and labels on a map.
#'
#' @param x object of class \code{sf}, \code{sfc} or \code{SpatRaster}
#' @param pos labels positions ("bottom", "left", "top" and / or "right")
#' @param cex labels size
#' @param col graticules and label color
#' @param lwd graticules line width
#' @param lty graticules line type
#' @param expandBB fractional values to expand the bounding box with, in each
#' direction (bottom, left, top, right)
#' @param label whether to add labels (TRUE) or not (FALSE)
#' @param add whether to add the layer to an existing plot (TRUE) or
#' not (FALSE)
#' @md
#' @section Use of graticules:
#' From \code{\link[sf:st_graticule]{st_graticule}}:
#' "In cartographic visualization, the use of graticules is not advised, unless
#' the graphical output will be used for measurement or navigation, or the
#' direction of North is important for the interpretation of the content, or
#' the content is intended to display distortions and artifacts created by
#' projection. Unnecessary use of graticules only adds visual clutter but
#' little relevant information. Use of coastlines, administrative boundaries
#' or place names permits most viewers of the output to orient themselves
#' better than a graticule."

#' @return An (invisible) layer of graticules is returned (LINESTRING).
#' @export
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq, expandBB = c(0, .1, .1, 0))
#' mf_graticule(mtq)
#'
#' mf_graticule(
#'   x = mtq,
#'   col = "coral4",
#'   lwd = 2,
#'   lty = 2,
#'   expandBB = c(.1, 0, 0, .1),
#'   label = TRUE,
#'   pos = c("right", "bottom"),
#'   cex = .8,
#'   add = FALSE
#' )
#' mf_map(mtq, add = TRUE)
mf_graticule <- function(x,
                         col = col,
                         lwd = 1,
                         lty = 1,
                         expandBB = rep(0, 4),
                         label = TRUE,
                         pos = c("top", "left"),
                         cex = .7,
                         add = TRUE) {
  if (missing(col)) {
    col <- getOption("mapsf.fg")
  }
  if (add == FALSE) {
    mf_init(x, expandBB = expandBB)
  }

  g <- sf::st_graticule(st_as_sfc(st_bbox(x)))

  mf_map(
    g,
    col = col,
    lwd = lwd,
    lty = lty,
    add = TRUE
  )
  if (label == TRUE) {
    invisible(lapply(seq_len(nrow(g)), function(i) {
      if (g$type[i] == "N" &&
        g$x_start[i] - min(g$x_start) < 1000 &&
        "left" %in% pos) {
        text(
          g[i, "x_start"],
          g[i, "y_start"],
          labels = parse(text = g[i, "degree_label"]),
          srt = g$angle_start[i],
          pos = 2,
          cex = cex,
          xpd = TRUE,
          col = col
        )
      }
      if (g$type[i] == "E" &&
        g$y_start[i] - min(g$y_start) < 1000 &&
        "bottom" %in% pos) {
        text(
          g[i, "x_start"],
          g[i, "y_start"],
          labels = parse(text = g[i, "degree_label"]),
          srt = g$angle_start[i] - 90,
          pos = 1,
          cex = cex,
          xpd = TRUE,
          col = col
        )
      }
      if (g$type[i] == "N" &&
        g$x_end[i] - max(g$x_end) > -1000 && "right" %in% pos) {
        text(
          g[i, "x_end"],
          g[i, "y_end"],
          labels = parse(text = g[i, "degree_label"]),
          srt = g$angle_end[i],
          pos = 4,
          cex = cex,
          xpd = TRUE,
          col = col
        )
      }
      if (g$type[i] == "E" &&
        g$y_end[i] - max(g$y_end) > -1000 && "top" %in% pos) {
        text(
          g[i, "x_end"],
          g[i, "y_end"],
          labels = parse(text = g[i, "degree_label"]),
          srt = g$angle_end[i] - 90,
          pos = 3,
          cex = cex,
          xpd = TRUE,
          col = col
        )
      }
    }))
  }
  return(invisible(g))
}
