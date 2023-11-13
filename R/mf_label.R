#' @title Plot labels
#' @description Put labels on a map.
#' @name mf_label
#' @eval my_params(c('x', 'var'))
#' @param col labels color, it can be a single color or a vector of colors
#' @param cex labels cex, it can be a single size or a vector of sizes
#' @param ... further \link{text} arguments.
#' @param bg halo color, it can be a single color or a vector of colors
#' @param r width of the halo, it can be a single value or a vector of values
#' @param overlap if FALSE, labels are moved so they do not overlap.
#' @param halo if TRUE, a 'halo' is displayed around the text and additional
#' arguments bg and r can be modified to set the color and width of the halo.
#' @param lines if TRUE, then lines are plotted between x,y and the word,
#' for those words not covering their x,y coordinate
#' @param q quality of the non overlapping labels placement. Possible values
#' are 0 (quick results), 1 (reasonable quality and speed), 2 (better quality),
#' 3 (insane quality, can take a lot of time).
#' @return No return value, labels are displayed.
#' @export
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mtq$cex <- c(rep(.8, 8), 2, rep(.8, 25))
#' mf_label(
#'   x = mtq, var = "LIBGEO",
#'   col = "grey10", halo = TRUE, cex = mtq$cex,
#'   overlap = FALSE, lines = FALSE
#' )
mf_label <- function(x,
                     var,
                     col,
                     cex = 0.7,
                     overlap = TRUE,
                     lines = TRUE,
                     halo = FALSE,
                     bg,
                     r = 0.1,
                     q = 1,
                     ...) {
  test_cur_plot()
  # margins mgmt
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))

  if (missing(col)) {
    col <- getOption("mapsf.fg")
  }
  if (missing(bg)) {
    bg <- getOption("mapsf.bg")
  }
  words <- x[[var]]
  cc <- sf::st_coordinates(sf::st_centroid(
    x = sf::st_geometry(x),
    of_largest_polygon = TRUE
  ))

  if (nrow(x) == 1) {
    overlap <- TRUE
  }

  if (!overlap) {
    xo <- unlist(cc[, 1])
    yo <- unlist(cc[, 2])
    lay <- wordlayout(xo, yo, words, cex, q = q, ...)
    if (lines) {
      nlab <- length(xo)
      if (length(col) != nlab) {
        col <- rep(col[1], nlab)
      }
      for (i in seq_along(xo)) {
        xl <- lay[i, 1]
        yl <- lay[i, 2]
        w <- lay[i, 3]
        h <- lay[i, 4]
        if (xo[i] < xl || xo[i] > xl + w ||
          yo[i] < yl || yo[i] > yl + h) {
          points(xo[i], yo[i], pch = 16, col = col[i], cex = .5)
          nx <- xl + .5 * w
          ny <- yl + .5 * h
          lines(c(xo[i], nx), c(yo[i], ny), col = col[i], lwd = 1)
        }
      }
    }
    cc <- matrix(
      data = c(lay[, 1] + .5 * lay[, 3], lay[, 2] + .5 * lay[, 4]),
      ncol = 2, byrow = FALSE
    )
  }
  if (halo) {
    shadowtext(
      x = cc[, 1], y = cc[, 2], labels = words,
      cex = cex, col = col, bg = bg, r = r, ...
    )
  } else {
    text(x = cc[, 1], y = cc[, 2], labels = words, cex = cex, col = col, ...)
  }
  return(invisible(x))
}
