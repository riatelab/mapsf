# Label placement
#' @name wordlayout
#' @title wordlayout
#' @description wordlayout
#' @param x long
#' @param y lat
#' @param words labels
#' @param cex cex
#' @param q quality
#' @return coords
#' @noRd
wordlayout <- function(x, y, words, cex = 1, q) {
  d <- data.frame(q = c(0, 1, 2, 3), v = c(1, .2, .1, .01))
  tstep <- rstep <- d[d$q == q, 2]
  tails <- "g|j|p|q|y"
  n <- length(words)
  sdx <- sd(x, na.rm = TRUE)
  sdy <- sd(y, na.rm = TRUE)
  if (sdx == 0) sdx
  if (sdy == 0) sdy
  if (length(cex) == 1) cex <- rep(cex, n)
  boxes <- list()
  for (i in seq_along(words)) {
    r <- 0
    theta <- runif(1, 0, 2 * pi)
    x1 <- xo <- x[i]
    y1 <- yo <- y[i]
    wid <- strwidth(words[i], cex = cex[i]) +
      0.4 * strwidth("R", cex = cex[i])
    ht <- strheight(words[i], cex = cex[i]) +
      0.4 * strheight("R", cex = cex[i])
    # mind your ps and qs
    if (grepl(tails, words[i])) {
      ht <- ht + ht * .2
    }

    while (TRUE) {
      if (noverlap(x1 - .5 * wid, y1 - .5 * ht, wid, ht, boxes)) {
        boxes[[length(boxes) + 1]] <- c(x1 - .5 * wid, y1 - .5 * ht, wid, ht)
        break
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

noverlap <- function(x1, y1, sw1, sh1, boxes) {
  for (i in seq_along(boxes)) {
    bnds <- boxes[[i]]
    x2 <- bnds[1]
    y2 <- bnds[2]
    sw2 <- bnds[3]
    sh2 <- bnds[4]
    if (x1 < x2) {
      overlap <- (x1 + sw1) > x2
    } else {
      overlap <- (x2 + sw2) > x1
    }
    if (y1 < y2) {
      overlap <- overlap && (y1 + sh1) > y2
    } else {
      overlap <- overlap && (y2 + sh2) > y1
    }
    if (overlap) {
      return(FALSE)
    }
  }
  return(TRUE)
}
