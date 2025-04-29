#' @title Get color palettes
#' @description \code{mf_get_pal} builds sequential, diverging and
#' qualitative color palettes.
#' Diverging color palettes can be dissymmetric (different number of colors in
#' each of the two gradients).
#' @name mf_get_pal
#' @param n the number of colors (>= 1) to be in the palette
#' @param palette a valid palette name. See \link{hcl.pals} to get available
#' palette names. The name is matched
#' to the list of available palettes, ignoring upper vs. lower case, spaces,
#' dashes, etc. in the matching.
#' @param alpha an alpha-transparency level in the range [0,1] (0 means
#' transparent and 1 means opaque)
#' @param rev	logical indicating whether the ordering of the colors should be
#' reversed
#' @param breaks a vector of class limit
#' @param mid a numeric value use to divide the palette in two colors
#' @param neutral a color, if two gradients are used, the 'neutral' color can be
#' added between them
#' @return A vector of colors.
#' @importFrom grDevices hcl.colors
#' @export
#' @examples
#' cls <- mf_get_pal(n = c(3, 7), palette = c("Reds 2", "Greens"))
#' plot(1:10, rep(1, 10), bg = cls, pch = 22, cex = 4)
#' mtq <- mf_get_mtq()
#' bks <- mf_get_breaks(mtq$MED, breaks = "equal", nbreaks = 8)
#' pal <- mf_get_pal(
#'   breaks = bks, mid = 15000,
#'   palette = c("Dark Mint", "Burg"), neutral = "grey90"
#' )
#' mf_map(mtq, "MED", "choro", breaks = bks, pal = pal)
#' pal <- mf_get_pal(breaks = bks, mid = bks[4], palette = c("Dark Mint", "Burg"))
#' mf_map(mtq, "MED", "choro", breaks = bks, pal = pal)
mf_get_pal <- function(n, palette, alpha = NULL,
                       rev = c(FALSE, FALSE), neutral,
                       breaks, mid) {
  if (missing(n) && !missing(breaks) && !missing(mid)) {
    pal <- mf_get_divpal(breaks, mid,
      palette = palette, neutral = neutral,
      alpha = alpha, rev = rev
    )
  } else {
    if (length(n) == 1) {
      pal <- hcl.colors(
        n = n, palette = palette,
        alpha = alpha, rev = rev[1]
      )
    }
    if (length(n) == 2) {
      nmax <- max(n)
      if (length(alpha) == 1) {
        alpha <- c(alpha, alpha)
      }
      pal1 <- hcl.colors(
        n = nmax, palette = palette[1],
        alpha = alpha[1], rev = rev[1]
      )
      pal2 <- hcl.colors(
        n = nmax, palette = palette[2],
        alpha = alpha[2], rev = rev[2]
      )

      if (n[1] < nmax) {
        pal1 <- pal1[(nmax - n[1] + 1):nmax]
      } else {
        pal1 <- pal1[1:nmax]
      }
      pal2 <- rev(pal2)
      pal2 <- pal2[1:n[2]]
      pal <- c(pal1, pal2)
      if (!missing(neutral)) {
        if (!is.null(alpha)) {
          neutral <- get_hex_pal(neutral, alpha[1])
        }
        pal <- c(pal1, neutral, pal2)
      }
    }
  }
  return(pal)
}


mf_get_divpal <- function(bks, mid, palette,
                          neutral,
                          alpha = NULL,
                          rev = c(FALSE, FALSE)) {
  if (missing(neutral)) {
    neutral <- "grey80"
  }
  if (length(palette) != 2) {
    stop("2 palette names must be provided.", call. = FALSE)
  }
  if (is.null(alpha)) {
    alpha <- 1
  }
  if (length(alpha) == 1) {
    alpha <- c(alpha, alpha)
  }
  bks <- sort(unique(bks))
  ncol <- length(bks) - 1
  if (mid %in% bks) {
    neg <- which(bks == mid) - 1
    pos <- ncol - neg
    if (neg == 0) {
      pal <- mf_get_pal(
        n = pos,
        palette = palette[2],
        alpha = alpha[2],
        rev = rev[2]
      )
    } else if (pos == 0) {
      pal <- mf_get_pal(
        n = neg,
        palette = palette[1],
        alpha = alpha[1],
        rev = rev[1]
      )
    } else {
      pal <- mf_get_pal(
        n = c(neg, pos),
        palette = palette,
        alpha = alpha,
        rev = rev
      )
    }
  } else {
    bks <- sort(c(bks, mid))
    bks_p <- bks[bks > mid]
    bks_n <- bks[bks < mid]
    lbp <- length(bks_p)
    lbn <- length(bks_n)
    if (lbp > 1 && lbn > 1) {
      pal <- mf_get_pal(
        n = c(lbn, lbp) - 1,
        palette = palette,
        alpha = alpha,
        rev = rev,
        neutral = neutral
      )
    } else if (lbn == 0) {
      pal <- mf_get_pal(
        n = lbp - 1,
        palette = palette[2],
        alpha = alpha,
        rev = rev
      )
    } else if (lbp == 0) {
      pal <- mf_get_pal(
        n = lbn - 1,
        palette = palette[1],
        alpha = alpha,
        rev = rev
      )
    } else if (lbn == 1) {
      pal <- c(
        get_hex_pal(neutral, alpha[1]),
        mf_get_pal(
          n = lbp - 1,
          palette = palette[2],
          alpha = alpha,
          rev = rev
        )
      )
    } else if (lbp == 1) {
      pal <- c(
        mf_get_pal(
          n = lbn - 1,
          palette = palette[1],
          alpha = alpha,
          rev = rev
        ),
        get_hex_pal(neutral, alpha[1])
      )
    }
  }
  return(pal)
}
