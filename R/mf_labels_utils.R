# shadow around the labels
#' @name shadowtext
#' @title shadowtext
#' @description shadowtext
#' @param x lon
#' @param y lat
#' @param labels labels
#' @param col col
#' @param bg bg
#' @param theta number of iteration
#' @param r radius
#' @param ... other txt params
#' @noRd
shadowtext <- function(x, y = NULL, labels, col = "white", bg = "black",
                       theta = seq(0, 2 * pi, length.out = 50), r = 0.1, ...) {
  xo <- r * strwidth("A")
  yo <- r * strheight("A")
  for (i in theta) {
    text(x + cos(i) * xo, y + sin(i) * yo, labels, col = bg, ...)
  }
  text(x, y, labels, col = col, ...)
}

# Label placement
#' @name wordlayout
#' @title wordlayout
#' @description wordlayout
#' @param x long
#' @param y lat
#' @param words labels
#' @param cex cex
#' @param xlim xlim
#' @param ylim ylim
#' @param tstep tstep
#' @param rstep rstep
#' @param ... other stuf
#' @return coords
#' @noRd
wordlayout <- function(x, y, words, cex = 1,
                       xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
                       tstep = .1, rstep = .1, ...) {
  tails <- "g|j|p|q|y"
  n <- length(words)
  sdx <- sd(x, na.rm = TRUE)
  sdy <- sd(y, na.rm = TRUE)
  if (sdx == 0) {
    sdx <- 1
  }
  if (sdy == 0) {
    sdy <- 1
  }
  if (length(cex) == 1) {
    cex <- rep(cex, n)
  }
  boxes <- list()
  for (i in seq_along(words)) {
    r <- 0
    theta <- runif(1, 0, 2 * pi)
    x1 <- xo <- x[i]
    y1 <- yo <- y[i]
    wid <- strwidth(words[i], cex = cex[i], ...) + 0.4 *
      strwidth("R", cex = cex[i], ...)
    ht <- strheight(words[i], cex = cex[i], ...) + 0.4 *
      strheight("R", cex = cex[i], ...)

    # mind your ps and qs
    if (grepl(tails, words[i])) {
      ht <- ht + ht * .2
    }
    is_overlaped <- TRUE
    while (is_overlaped) {
      if (!is_overlap(x1 - .5 * wid, y1 - .5 * ht, wid, ht, boxes) &&
        x1 - .5 * wid > xlim[1] && y1 - .5 * ht > ylim[1] &&
        x1 + .5 * wid < xlim[2] && y1 + .5 * ht < ylim[2]) {
        boxes[[length(boxes) + 1]] <- c(x1 - .5 * wid, y1 - .5 * ht, wid, ht)
        is_overlaped <- FALSE
      } else {
        theta <- theta + tstep
        r <- r + rstep * tstep / (2 * pi)
        x1 <- xo + sdx * r * cos(theta)
        y1 <- yo + sdy * r * sin(theta)
      }
    }
  }
  result <- do.call(rbind, boxes)
  colnames(result) <- c("x", "y", "width", "ht")
  rownames(result) <- words
  result
}
