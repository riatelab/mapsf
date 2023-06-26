#' @title Plot an annotation
#' @description Plot an annotation on a map.
#' @param x an sf object with 1 row, a couple of coordinates (c(x, y)) or
#' "interactive"
#' @param txt the text to display
#' @param pos position of the text, one of "topleft", "topright", "bottomright",
#' "bottomleft"
#' @param cex size of the text
#' @param col_arrow arrow color
#' @param col_txt text color
#' @param halo add a halo around the text
#' @param bg halo color
#' @param s arrow size (min=1)
#' @param ... further \link{text} arguments.
#' @export
#' @return No return value, an annotation is displayed.
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_annotation(
#'   x = c(711167.8, 1614764),
#'   txt = "Look!\nImportant feature\nhere!",
#'   pos = "bottomleft", cex = 1.2, font = 2,
#'   halo = TRUE, s = 1.5
#' )
#'
#' mf_annotation(
#'   x = mtq[20, ],
#'   txt = "This is less\nimportant",
#'   cex = .7, font = 3, s = 1.3
#' )
mf_annotation <- function(x, txt, pos = "topright",
                          cex = 0.8, col_arrow,
                          col_txt, halo = FALSE, bg, s = 1, ...) {
  test_cur_plot()
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))

  if (missing(col_arrow)) {
    col_arrow <- "black"
  }
  if (missing(col_txt)) {
    col_txt <- getOption("mapsf.fg")
  }
  if (missing(bg)) {
    bg <- getOption("mapsf.bg")
  }


  if (inherits(x, "character") && length(x)) {
    x <- interleg(txt = c("annotation", "Annotation"))
  }

  if (inherits(x, c("sf", "sfc"))) {
    xy <- sf::st_coordinates(
      sf::st_centroid(
        sf::st_geometry(x[1, ]),
        of_largest_polygon = TRUE
      )
    )
  }
  if (inherits(x, "numeric") && length(x) == 2) {
    xy <- x
  }

  if (s < 1) s <- 1

  inset <- strwidth("M", units = "user", cex = 1) / 2
  radius <- 5 * s * inset

  switch(pos,
    topright = {
      drawarc(
        x = xy[1] + radius,
        y = xy[2] + inset,
        radius = radius,
        deg1 = 180,
        deg2 = 90,
        col = col_arrow
      )
      polygon(
        x = c(
          xy[1] - inset / 3,
          xy[1],
          xy[1] + inset / 3,
          xy[1] - inset / 3
        ),
        y = c(
          xy[2] + 2 * inset,
          xy[2] + 5 * inset / 6,
          xy[2] + 2 * inset,
          xy[2] + 2 * inset
        ),
        col = col_arrow, border = col_arrow, lwd = 1.2
      )
      if (halo) {
        shadowtext(
          x = xy[1] + radius + inset / 2,
          y = xy[2] + radius + inset,
          labels = txt, col = col_txt, bg = bg,
          cex = cex, adj = c(0, .5), ...
        )
      } else {
        text(
          x = xy[1] + radius + inset / 2,
          y = xy[2] + radius + inset, cex = cex,
          labels = txt, col = col_txt,
          adj = c(0, .5), ...
        )
      }
    },
    topleft = {
      drawarc(
        x = xy[1] - radius,
        y = xy[2] + inset,
        radius = radius,
        deg1 = 0,
        deg2 = 90,
        col = col_arrow
      )
      polygon(
        x = c(
          xy[1] - inset / 3,
          xy[1],
          xy[1] + inset / 3,
          xy[1] - inset / 3
        ),
        y = c(
          xy[2] + 2 * inset,
          xy[2] + 5 * inset / 6,
          xy[2] + 2 * inset,
          xy[2] + 2 * inset
        ),
        col = col_arrow, border = col_arrow, lwd = 1.2
      )
      if (halo) {
        shadowtext(
          x = xy[1] - radius - inset / 2,
          y = xy[2] + radius + inset,
          labels = txt, col = col_txt, bg = bg,
          cex = cex, adj = c(1, .5), ...
        )
      } else {
        text(
          x = xy[1] - radius - inset / 2,
          y = xy[2] + radius + inset, cex = cex,
          labels = txt, col = col_txt,
          adj = c(1, .5), ...
        )
      }
    },
    bottomleft = {
      drawarc(
        x = xy[1] - radius,
        y = xy[2] - inset,
        radius = radius,
        deg1 = 360,
        deg2 = 270,
        col = col_arrow
      )
      polygon(
        x = c(
          xy[1] - inset / 3,
          xy[1],
          xy[1] + inset / 3,
          xy[1] - inset / 3
        ),
        y = c(
          xy[2] - 2 * inset,
          xy[2] - 5 * inset / 6,
          xy[2] - 2 * inset,
          xy[2] - 2 * inset
        ),
        col = col_arrow, border = col_arrow, lwd = 1.2
      )
      if (halo) {
        shadowtext(
          x = xy[1] - radius - inset / 2,
          y = xy[2] - radius - inset,
          labels = txt, col = col_txt, bg = bg,
          cex = cex, adj = c(1, .5), ...
        )
      } else {
        text(
          x = xy[1] - radius - inset / 2,
          y = xy[2] - radius - inset, cex = cex,
          labels = txt, col = col_txt,
          adj = c(1, .5), ...
        )
      }
    },
    bottomright = {
      drawarc(
        x = xy[1] + radius,
        y = xy[2] - inset,
        radius = radius,
        deg1 = 270,
        deg2 = 180,
        col = col_arrow
      )
      polygon(
        x = c(
          xy[1] - inset / 3,
          xy[1],
          xy[1] + inset / 3,
          xy[1] - inset / 3
        ),
        y = c(
          xy[2] - 2 * inset,
          xy[2] - 5 * inset / 6,
          xy[2] - 2 * inset,
          xy[2] - 2 * inset
        ),
        col = col_arrow, border = col_arrow, lwd = 1.2
      )
      if (halo) {
        shadowtext(
          x = xy[1] + radius + inset / 2,
          y = xy[2] - radius - inset,
          labels = txt, col = col_txt, bg = bg,
          cex = cex, adj = c(0, .5), ...
        )
      } else {
        text(
          x = xy[1] + radius + inset / 2,
          y = xy[2] - radius - inset, cex = cex,
          labels = txt, col = col_txt,
          adj = c(0, .5), ...
        )
      }
    }
  )
}



drawarc <- function(x = 1, y = NULL, radius = 1, deg1 = 0, deg2 = 45, col) {
  n <- 0.05
  angle1 <- deg1 * pi / 180
  angle2 <- deg2 * pi / 180
  lwd <- 1.2
  xylim <- par("usr")
  ymult <- 1
  devunits <- grDevices::dev.size("px")
  xy <- grDevices::xy.coords(x, y)
  x <- xy$x
  y <- xy$y
  a1 <- pmin(angle1, angle2)
  a2 <- pmax(angle1, angle2)
  angle1 <- a1
  angle2 <- a2
  args <- data.frame(
    x, y, radius, angle1, angle2, n, col, lwd, xylim,
    devunits, ymult
  )
  for (i in seq_len(nrow(args))) {
    do.call(draw_arc_0, c(args[i, ]))
  }
}


draw_arc_0 <- function(x, y, radius, angle1, angle2, n, col, lwd, xylim,
                       devunits, ymult, ...) {
  delta_angle <- (angle2 - angle1)
  if (n != as.integer(n)) {
    n <- as.integer(1 + delta_angle / n)
  }
  delta_angle <- delta_angle / n
  angle_s <- angle1 + seq(0, length = n) * delta_angle
  angle_e <- c(angle_s[-1], angle2)
  if (n > 1) {
    half_lwd_user <- (lwd / 2) * (xylim[2] - xylim[1]) / devunits[1]
    adj_angle <- delta_angle * half_lwd_user /
      (2 * (radius + half_lwd_user))
    angle_s[2:n] <- angle_s[2:n] - adj_angle
    angle_e[1:(n - 1)] <- angle_e[1:(n - 1)] + adj_angle
  }
  p1x <- x + radius * cos(angle_s)
  p1y <- y + radius * sin(angle_s) * ymult
  p2x <- x + radius * cos(angle_e)
  p2y <- y + radius * sin(angle_e) * ymult
  segments(p1x, p1y, p2x, p2y, col = col, lwd = lwd, lend = 3)
}
