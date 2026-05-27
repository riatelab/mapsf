#' @title Plot a distribution
#' @description
#' This function displays the statistical distribution of a variable with
#' a histogram, a box plot, a strip chart and a density curve on the same plot.
#'
#' This graphic can be useful to choose an appropriate classification method for
#' choropleth maps.
#'
#' User-defined class boundaries can also be displayed on the plot.
#'
#' @param x a numeric variable
#' @param nbins number of bins in the histogram
#' @param bw bandwidth of the density curve
#' @param breaks a vector of class boundaries. If `breaks` is used, the boxplot
#' is not displayed.
#' @param pal a color, a set of colors (hex codes) or a palette name.
#' Palette names can be obtained with [hcl.pals]. The default color is either
#' the highlight color if `breaks` is not used, or the background color
#' otherwise (see [mf_theme]).
#' @param rev logical indicating whether the ordering of the colors should be
#' reversed
#' @param alpha opacity, in the range \[0,1\] (0 means
#' transparent and 1 means opaque). Default is set to 1.
#' @param main plot title
#' @param yaxt if FALSE the y axis is not displayed
#' @param ylab y axis label
#' @return The number of bins of the histogram and the bandwidth of the density
#' curve are (invisibly) returned in a list.
#' @export
#' @seealso [mf_map_choro]
#' @examples
#' (mf_distr(rnorm(1000)))
#' mf_distr(rbeta(1000, .6, 7))
#' mf_distr(rbeta(1000, 5, .6))
#' a <- rbeta(1000, .6, 7)
#' bks <- mf_get_breaks(a, nbreaks = 5, breaks = "quantile")
#' mf_distr(a, breaks = bks)
#' mf_distr(a,
#'   breaks = bks, pal = "Teal", yaxt = FALSE,
#'   main = 'Classification method : "quantile"'
#' )
mf_distr <- function(x, nbins, bw, breaks, pal, alpha = 1, rev = FALSE,
                     main = "Distribution", yaxt = TRUE, ylab = "Density") {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  x <- x[is.finite(x)]

  if (missing(bw) && missing(nbins)) {
    bw <- bw.SJ(x)
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

  hbks <- seq(min(x), max(x), length.out = nbins + 1)

  # graphic basic parameters
  d <- density(x, bw = bw)
  h <- hist(x, breaks = hbks, plot = FALSE)
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
  set.seed(46)
  pts_y <- runif(d$n, min = y_lim[1], max = y_lim[1] + large_offset)

  # curve coords
  dx <- d$x[d$x >= x_lim[1] & d$x <= x_lim[2]]
  dy <- d$y[d$x >= x_lim[1] & d$x <= x_lim[2]]

  # actual plot
  # empty
  plot(
    x = x_lim,
    y = y_lim,
    col = NA,
    axes = FALSE,
    xlab = "",
    ylab = ""
  )
  # background
  rcol <- getOption("mapsf.background")
  rborder <- getOption("mapsf.background")

  recordGraphics(
    {
      plt <- par("plt")
      usr <- par("usr")
      rect(
        par("usr")[1] - plt[1] * (usr[2] - usr[1]) / (plt[2] - plt[1]),
        par("usr")[3] - plt[3] * (usr[4] - usr[3]) / (plt[4] - plt[3]),
        par("usr")[2] + (1 - plt[2]) * (usr[2] - usr[1]) / (plt[2] - plt[1]),
        par("usr")[4] + (1 - plt[4]) * (usr[4] - usr[3]) / (plt[4] - plt[3]),
        col = rcol,
        border = rborder,
        xpd = TRUE
      )
    },
    list = list(rcol = rcol, rborder = rborder),
    env = getNamespace("mapsf")
  )

  # histogram
  hist(
    x,
    breaks = h$breaks,
    freq = FALSE,
    ylim = y_lim,
    xlim = x_lim,
    lwd = 1,
    axes = FALSE,
    xlab = "",
    main = "Distribution",
    col.lab = getOption("mapsf.highlight"),
    col.main = getOption("mapsf.highlight"),
    col.axis = getOption("mapsf.highlight"),
    col = getOption("mapsf.foreground"),
    border = getOption("mapsf.background"),
    lwd = .2,
    add = TRUE
  )
  title(
    main = main,
    ylab = ifelse(yaxt, ylab, ""),
    line = 2,
    col.lab = getOption("mapsf.highlight"),
    col.main = getOption("mapsf.highlight")
  )
  # smooth line
  lines(
    x = dx,
    y = dy,
    lwd = 2,
    col = go(opt = "highlight", legacy = "#cf0000")
  )
  axis(
    side = 1,
    at = x_labels,
    col.axis = getOption("mapsf.highlight"),
    col = getOption("mapsf.highlight"),
    col.ticks = getOption("mapsf.highlight")
  )

  if (isTRUE(yaxt)) {
    axis(
      side = 2,
      pos = x_lim[1],
      at = y_labels,
      las = 2,
      col.axis = getOption("mapsf.highlight"),
      col = getOption("mapsf.highlight"),
      col.ticks = getOption("mapsf.highlight")
    )
  }

  if (!missing(breaks)) {
    if (missing(pal)) {
      pal <- getOption("mapsf.foreground")
    }
    pal <- get_the_pal(pal = pal, nbreaks = length(breaks) - 1, alpha = alpha, rev = !rev)
    mycols <- get_col_vec(x = x, breaks = breaks, pal = pal, jen = FALSE)
    points(
      x = x,
      y = pts_y,
      pch = 21,
      cex = .7,
      lwd = .8,
      bg = mycols,
      col = getOption("mapsf.background")
    )
    segments(
      x0 = breaks,
      y0 = rep(y_lim[1] - small_offset / 2, length(breaks)),
      x1 = breaks,
      y1 = rep(y_lim[1] + large_offset + small_offset / 2, length(breaks)),
      col = getOption("mapsf.highlight"),
      lwd = 2
    )
  } else {
    points(
      x = x,
      y = pts_y,
      pch = 21,
      cex = .7,
      lwd = .8,
      bg = getOption("mapsf.highlight"),
      col = getOption("mapsf.background")
    )

    box_plot(
      x = b$stats[, 1],
      y = y_lim[1],
      large_offset = large_offset,
      small_offset = small_offset,
      col = getOption("mapsf.background"),
      lwd = 4
    )
    box_plot(
      x = b$stats[, 1],
      y = y_lim[1],
      large_offset = large_offset,
      small_offset = small_offset,
      col = getOption("mapsf.highlight"),
      lwd = 1.5
    )
  }

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
