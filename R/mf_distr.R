#' @title Plot a distribution
#' @description
#' This function displays a histogram, a box plot, a strip chart
#' and a density curve on the same plot.
#'
#' @param x a numeric variable
#' @param nbins number of bins in the histogram
#' @param bw bandwidth of the density curve
#'
#' @return The number of bins of the histogram and the bandwidth of the density
#' curve are (invisibly) returned in a list.
#' @export
#'
#' @examples
#' (mf_distr(rnorm(1000)))
#' mf_distr(rbeta(1000, .6, 7))
#' mf_distr(rbeta(1000, 5, .6))
mf_distr <- function(x, nbins, bw) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  x <- x[is.finite(x)]

  if (missing(bw) && missing(nbins)) {
    bw <- stats::bw.SJ(x)
    nbins <- round((max(x) - min(x)) / (2 * bw), 0)
  }
  if (!missing(bw) && missing(nbins)) {
    nbins <- round((max(x) - min(x)) / (2 * bw), 0)
  }
  if (missing(bw) && !missing(nbins)) {
    bw <- ((max(x) - min(x)) / nbins) / 2
  }

  if (nbins > 100) {
    nbins <- 100
  }
  if (nbins < 3) {
    nbins <- 3
  }

  bks <- seq(min(x), max(x), length.out = nbins + 1)

  # graphic basic parameters
  d <- stats::density(x, bw = bw)
  h <- hist(x, breaks = bks, plot = FALSE)
  b <- boxplot(x, plot = FALSE)

  # graphic dimensions and labels
  y_labels <- pretty(c(min(d$y, h$density), max(d$y, h$density)))
  y_lim <- c(min(y_labels), max(y_labels))
  dim_y_lim <- diff(y_lim)
  small_offset <- dim_y_lim / 20
  large_offset <- dim_y_lim / 7
  y_lim[1] <- y_lim[1] - large_offset - small_offset
  x_labels <- pretty(x)
  x_lim <- c(min(x_labels), max(x_labels))

  # points coords
  pts_y <- runif(d$n, min = y_lim[1], max = y_lim[1] + large_offset)

  # curve coords
  dx <- d$x[d$x >= x_lim[1] & d$x <= x_lim[2]]
  dy <- d$y[d$x >= x_lim[1] & d$x <= x_lim[2]]

  # actual plot
  hist(
    x,
    breaks = h$breaks,
    freq = FALSE,
    ylim = y_lim,
    xlim = x_lim,
    axes = FALSE,
    xlab = "",
    main = "Distribution",
    col = "grey20",
    border = "grey80",
    lwd = .2
  )
  lines(x = dx, y = dy, lwd = 2, col = "#cf0000")
  axis(side = 1, at = x_labels)
  axis(
    side = 2,
    pos = x_lim[1],
    at = y_labels,
    las = 2
  )
  points(
    x = x,
    y = pts_y,
    pch = 21,
    cex = .5,
    bg = "grey30",
    col = "grey70"
  )

  box_plot(
    x = b$stats[, 1], y = y_lim[1], large_offset = large_offset,
    small_offset = small_offset, col = "white", lwd = 2
  )
  box_plot(
    x = b$stats[, 1], y = y_lim[1], large_offset = large_offset,
    small_offset = small_offset, col = "black", lwd = 1.2
  )

  return(invisible(list(bw = bw, nbins = round(nbins, 0))))
}

box_plot <- function(x, y, large_offset, small_offset, col, lwd) {
  # box
  polygon(
    x = x[c(2, 4, 4, 2)],
    y = c(
      y - small_offset / 2,
      y - small_offset / 2,
      y + large_offset + small_offset / 2,
      y + large_offset + small_offset / 2
    ),
    lwd = lwd,
    border = col
  )
  # Moustaches
  y_mid <- y + large_offset / 2
  segments(
    x0 = c(x[2], x[4]),
    y0 = c(y_mid, y_mid),
    x1 = c(x[1], x[5]),
    y1 = c(y_mid, y_mid),
    lty = 1,
    lwd = lwd,
    col = col
  )
  # Median
  segments(
    x0 = x[3],
    y0 = y - small_offset / 2,
    x1 = x[3],
    y1 = y[1] + large_offset + small_offset / 2,
    lty = 1,
    lwd = lwd + .8,
    col = col
  )
}
