# get prop symbols size and dim  from topleft corner
get_xy_s <- function(x, y, val, inches, symbol) {
  sizes <- get_size(
    var = val,
    inches = inches,
    val_max = max(val),
    symbol = symbol
  )
  sizesi <- xinch(sizes)
  x <- rep(x + sizesi[1], length(val))
  y <- y - sizesi[1] * 2 + sizesi
  h <- sizesi[1] * 2
  w <- h

  return(list(x = x, y = y, s = sizesi, h = h, w = w))
}

# lines from top of symbols to labels
get_xy_lines <- function(x, y, sizesi, inset) {
  x0 <- rep(x, length(sizesi))
  x1 <- rep(x + sizesi[1] + inset, length(sizesi))
  y0 <- y1 <- y + sizesi
  return(list(x0 = x0, x1 = x1, y0 = y0, y1 = y1, w = x1[1] - x0[1] + inset))
}

# get labels position
get_xy_lab_s <- function(x, y, val, val_cex) {
  w <- max(strwidth(val, units = "user", cex = val_cex, font = 1))
  y <- rev(y)
  return(list(x = x, y = y, w = w))
}

# box around the legend
get_xy_rect_s <- function(xy_title, xy_symbols, xy_lines, xy_lab, inset) {
  xy_leg <- list(
    xleft = xy_title$x,
    ybottom = xy_title$y - inset / 2 - xy_symbols$h,
    xright =
      xy_title$x +
        max(
          xy_title$w,
          xy_symbols$h / 2 +
            xy_lines$w +
            xy_lab$w
        ),
    ytop = xy_title$y + xy_title$h
  )
  xy_leg
}



self_adjust <- function(var, inches, val_cex) {
  # get min & max
  val <- c(min(var), max(var))
  # factors
  b <- c(5, 2.5, 1)
  # min val
  min_s <- min(val)
  # max val
  max_s <- max(val)
  # get candidat values for the legend
  ndmax <- floor(log10(max_s))
  if (min_s < 1) {
    ndmin <- nchar(as.character(signif(min_s, digits = 0))) - 2
  } else {
    ndmin <- 1
  }
  i <- c(-ndmin:ndmax)
  v <- vector("numeric", 0)
  for (base in b) {
    v <- c(v, base * 10^i)
  }

  v <- c(max_s, min_s, v)
  v <- v[v >= min_s]
  v <- v[v <= max_s]
  v <- sort(unique(v))

  # circle sizes in map units for candidate values
  si <- xinch(get_size(
    var = v, inches = inches,
    val_max = max(val), symbol = "circle"
  ))
  # texte size labels in map units
  h <- max(strheight(val, units = "user", cex = val_cex, font = 1)) * 1.2

  # number of candidate values
  i <- length(si)

  # vector of displayed values
  a <- vector("logical", i)

  # The last one (max) is always displayed
  a[i] <- TRUE

  # go to next one
  i <- i - 1
  while (TRUE) {
    maxv <- si[length(si)] * 2
    # test space between two circles
    if (maxv - si[i] * 2 <= h) {
      # the space is too small
      a[i] <- FALSE
      # go to next value
      si <- si[-(length(si) - 1)]
    } else {
      # the space is not too small
      si <- si[-length(si)]
      # display ok
      a[i] <- TRUE
    }
    # increment
    i <- i - 1
    # last value
    if (i == 0) break
  }

  # If only one value is selected (max) select also the lower
  if (sum(a) <= 1) {
    a[1] <- TRUE
  }

  # if min value not selected, remove min selected value and replace
  # with min value
  if (a[1] == FALSE) {
    a[which(a)[1]] <- FALSE
    a[1] <- TRUE
  }

  return(v[a])
}
