display_text <- function(x, y, txt, cex, font, family, adj, col_text,
                         offset_x, offset_y,
                         box, box_bbox, col_box, col_box_border, lwd_box,
                         halo, cex_halo, col_halo) {
  if (isTRUE(box)) {
    rect(
      xleft = box_bbox$xleft,
      ybottom = box_bbox$ybottom,
      xright = box_bbox$xright,
      ytop = box_bbox$ytop,
      col = col_box,
      border = col_box_border,
      lwd = lwd_box
    )
  }
  if (isTRUE(halo)) {
    theta <- seq(0, 2 * pi, length.out = 50)
    r <- .2 * cex_halo
    xo <- r * offset_x
    yo <- r * offset_y
    for (j in theta) {
      text(
        x = x + cos(j) * xo,
        y = y + sin(j) * yo * 1,
        labels = txt,
        cex = cex,
        font = font,
        family = family,
        adj = adj,
        col = col_halo
      )
    }
  }
  text(
    x = x,
    y = y,
    labels = txt,
    cex = cex,
    font = font,
    family = family,
    adj = adj,
    col = col_text
  )
}


xy_pos <- function(x, y, cex, offset_x, offset_y, pos) {
  if (pos == 0) {
    return(c(x, y))
  }

  df_angle <- data.frame(
    pos = 1:8,
    angle = c(270, 225, 180, 135, 90, 45, 0, 315)
  )
  angle <- df_angle[match(pos, df_angle[, 1]), "angle"]
  return(c(
    x = x + cex * offset_x * cos((angle * pi) / 180),
    y = y + cex * offset_y * sin((angle * pi) / 180)
  ))
}

get_position_from_name <- function(pos) {
  df_pos <- data.frame(
    named_pos = c(
      "center", "bottom", "bottomleft", "left", "topleft", "top",
      "topright", "right", "bottomright"
    ),
    pos = 0:8
  )
  posr <- df_pos[match(pos, df_pos[, 1]), "pos"][[1]]
  if (is.na(posr)) {
    warning(paste0("'", pos, "' is not a correct position."), call. = FALSE)
    posr <- 0
  }
  return(posr)
}

display_line <- function(line, pos, box, x, y, xy, offset_x, offset_y,
                         lwd, col_line, arrow, cex_arrow, offset,
                         htext, wtext, clockwise) {
  if (pos == 0 || line == 0) {
    return()
  }

  if (arrow == FALSE) {
    cex_arrow <- 0
  }

  xy0 <- xy
  xy1 <- xy_pos(
    x = x, y = y, cex = 1,
    offset_x = offset_x / 2,
    offset_y = offset_y / 2, pos = pos
  )


  if (line == 2) {
    if (pos %in% c(1, 3, 5, 7)) {
      line <- 1
    } else {
      if (clockwise == TRUE) {
        if (pos %in% c(4)) {
          xy0[2] <- xy0[2] + offset_y / 2 + htext / 2
        }
        if (pos %in% c(8)) {
          xy0[2] <- xy0[2] - offset_y / 2 - htext / 2
        }
        if (pos %in% c(4, 8)) {
          segments(
            x0 = xy0[1], y0 = xy0[2], x1 = x, y1 = xy0[2],
            col = col_line, lwd = lwd
          )
          arrows(
            x0 = x, y0 = xy0[2], x1 = x, y1 = xy1[2],
            lwd = lwd, col = col_line, length = 0.05 * cex_arrow
          )
        }
        if (pos %in% c(6)) {
          xy0[1] <- xy0[1] + offset_x / 2 + wtext / 2
        }
        if (pos %in% c(2)) {
          xy0[1] <- xy0[1] - offset_x / 2 - wtext / 2
        }
        if (pos %in% c(2, 6)) {
          segments(
            x0 = xy0[1], y0 = xy0[2], x1 = xy0[1], y1 = y,
            col = col_line, lwd = lwd
          )
          arrows(
            x0 = xy0[1], y0 = y, x1 = xy1[1], y1 = y,
            lwd = lwd, col = col_line, length = 0.05 * cex_arrow
          )
        }
      } else {
        if (pos %in% c(6)) {
          xy0[2] <- xy0[2] + offset_y / 2 + htext / 2
        }
        if (pos %in% c(2)) {
          xy0[2] <- xy0[2] - offset_y / 2 - htext / 2
        }
        if (pos %in% c(2, 6)) {
          segments(
            x0 = xy0[1], y0 = xy0[2], x1 = x, y1 = xy0[2],
            col = col_line, lwd = lwd
          )
          arrows(
            x0 = x, y0 = xy0[2], x1 = x, y1 = xy1[2],
            lwd = lwd, col = col_line, length = 0.05 * cex_arrow
          )
        }
        if (pos %in% c(8)) {
          xy0[1] <- xy0[1] + offset_x / 2 + wtext / 2
        }
        if (pos %in% c(4)) {
          xy0[1] <- xy0[1] - offset_x / 2 - wtext / 2
        }
        if (pos %in% c(4, 8)) {
          segments(
            x0 = xy0[1], y0 = xy0[2], x1 = xy0[1], y1 = y,
            col = col_line, lwd = lwd
          )
          arrows(
            x0 = xy0[1], y0 = y, x1 = xy1[1], y1 = y,
            lwd = lwd, col = col_line, length = 0.05 * cex_arrow
          )
        }
      }
    }
  }

  if (line == 3) {
    if (clockwise == TRUE) {
      if (pos == 2) {
        xy0[1] <- xy0[1] - offset_x / 2 - wtext / 2
        xy1[2] <- xy1[2] + offset_y / 4
      }
      if (pos == 4) {
        xy0[2] <- xy0[2] + offset_y / 2 + htext / 2
        xy1[1] <- xy1[1] + offset_x / 4
      }
      if (pos == 6) {
        xy0[1] <- xy0[1] + offset_x / 2 + wtext / 2
        xy1[2] <- xy1[2] - offset_y / 4
      }
      if (pos == 8) {
        xy0[2] <- xy0[2] - offset_y / 2 - htext / 2
        xy1[1] <- xy1[1] - offset_x / 4
      }
    } else {
      if (pos == 2) {
        xy0[2] <- xy0[2] - offset_y / 2 - htext / 2
        xy1[1] <- xy1[1] + offset_x / 4
      }
      if (pos == 4) {
        xy0[1] <- xy0[1] - offset_x / 2 - wtext / 2
        xy1[2] <- xy1[2] - offset_y / 4
      }
      if (pos == 6) {
        xy0[2] <- xy0[2] + offset_y / 2 + htext / 2
        xy1[1] <- xy1[1] - offset_x / 4
      }
      if (pos == 8) {
        xy0[1] <- xy0[1] + offset_x / 2 + wtext / 2
        xy1[2] <- xy1[2] + offset_y / 4
      }
    }
    trio <- build_trio(
      x0 = xy0[1], y0 = xy0[2], x1 = xy1[1], y1 = xy1[2],
      pos = pos, clockwise = clockwise
    )

    x_courbe <- bernstein(trio$x, seq(0, 1, .01))
    y_courbe <- bernstein(trio$y, seq(0, 1, .01))
    points(x_courbe, y_courbe, type = "l", col = col_line, lwd = lwd)
    arrows(
      x0 = x_courbe[100], y0 = y_courbe[100],
      x1 = x_courbe[101], y1 = y_courbe[101],
      lwd = lwd, col = col_line, length = 0.05 * cex_arrow
    )
  }

  if (line == 1) {
    arrows(
      x0 = xy0[1], y0 = xy0[2], x1 = xy1[1], y1 = xy1[2],
      lwd = lwd, col = col_line, length = 0.05 * cex_arrow
    )
  }
}


bernstein <- function(beta, t = seq(0, 1, .01)) {
  n <- length(beta) - 1
  w <- choose(n, 0:n)
  b <- rep(0, length(t))
  for (v in 0:n) {
    b <- b + beta[v + 1] * w[v + 1] * t^v * (1 - t)^(n - v)
  }
  b
}

build_trio <- function(x0, y0, x1, y1, pos, clockwise = FALSE) {
  f <- yinch(1) / xinch(1)
  k <- sqrt((x1 - x0)^2 + (y1 - y0)^2) / 5
  if (pos == 1) {
    kx <- k
    ky <- 0
  }
  if (pos == 2) {
    kx <- k
    ky <- -k * f
  }
  if (pos == 3) {
    kx <- 0
    ky <- -k * f
  }
  if (pos == 4) {
    kx <- -k
    ky <- -k * f
  }
  if (pos == 5) {
    kx <- -k
    ky <- 0
  }
  if (pos == 6) {
    kx <- -k
    ky <- k * f
  }
  if (pos == 7) {
    kx <- 0
    ky <- k * f
  }
  if (pos == 8) {
    kx <- k
    ky <- k * f
  }
  s <- ifelse(isTRUE(clockwise), -1, 1)

  xt <- x0 + (x1 - x0) / 2 + (kx * s)
  yt <- y0 + (y1 - y0) / 2 + (ky * s)

  return(list(x = c(x0, xt, x1), y = c(y0, yt, y1)))
}


get_xy_for_txt <- function(x, adj, pos, offset) {
  if (inherits(x, c("sf", "sfc"))) {
    x <- sf::st_coordinates(
      sf::st_centroid(
        sf::st_geometry(x[1, ]),
        of_largest_polygon = TRUE
      )
    )
  }
  if (is.numeric(x) && length(x) == 2) {
    return(list(x = x[[1]], y = x[[2]], pos = pos, offset = offset))
  }

  x_spacing <- xinch(par("csi")) / 4
  y_spacing <- yinch(par("csi")) / 4
  pu <- par("usr")
  extra <- adj * c(x_spacing, y_spacing)
  xy <- switch(x,
    bottomleft = list(
      x = pu[1],
      y = pu[3],
      pos = "topright"
    ),
    topleft = list(
      x = pu[1],
      y = pu[4],
      pos = "bottomright"
    ),
    left = list(
      x = pu[1],
      y = pu[3] + (pu[4] - pu[3]) / 2,
      pos = "right"
    ),
    top = list(
      x = pu[1] + (pu[2] - pu[1]) / 2,
      y = pu[4],
      pos = "bottom"
    ),
    bottom = list(
      x = pu[1] + (pu[2] - pu[1]) / 2,
      y = pu[3],
      pos = "top"
    ),
    bottomright = list(
      x = pu[2],
      y = pu[3],
      pos = "topleft"
    ),
    right = list(
      x = pu[2],
      y = pu[3] + (pu[4] - pu[3]) / 2,
      pos = "left"
    ),
    topright = list(
      x = pu[2],
      y = pu[4],
      pos = "bottomleft"
    )
  )
  xy$x <- xy$x + extra[1]
  xy$y <- xy$y + extra[2]
  xy$offset <- 0
  return(xy)
}
