#' @title Plot a text
#'
#' @description Plot a text on the map.
#'
#' @param x
#' an sf object (the first row is used),
#' a couple of coordinates (c(x, y)),
#' a position (one of 'topleft', 'top','topright', 'right', 'bottomright',
#' 'bottom', 'bottomleft', 'left'),
#' or "interactive" for interactive placement.
#' @param txt text to display
#' @param cex text size
#' @param col_txt text color, hex code or color name given by [colors].
#' The default color is the highlight color (see [mf_theme]).
#' @param pos position of the text relative to `x` (one of 'center',
#' 'topleft', 'top', 'topright', 'right', 'bottomright', 'bottom',
#' 'bottomleft', 'left').
#' Ignored if `x` is a position.
#' @param offset offset between the text and `x`. Ignored if `x` is a position.
#' @param align text alignement, one of "left", "right" or "center"
#' @param font text font
#' @param family text family
#' @param halo add a halo around the text
#' @param col_halo halo color, hex code or color name given by [colors].
#' The default color is the background color (see [mf_theme]).
#' @param cex_halo halo width
#' @param box add a box around the text
#' @param col_box box color, hex code or color name given by [colors].
#' The default color is the background color (see [mf_theme]).
#' @param col_box_border box border color, hex code or color name given by
#' [colors].
#' The default color is the highlight color (see [mf_theme]).
#' @param lwd_box line width of the box border
#' @param line type of the line drawn between the text and `x`.
#' 0 for no line, 1 for a straight line, 2 for a line with an angle,
#' 3 for a curved line. Ignored if `x` is a position.
#' @param clockwise direction of the curve for types 2 and 3
#' @param lwd line width
#' @param col_line line color, hex code or color name given by [colors].
#' The default color is the highlight color (see [mf_theme]).
#' @param arrow add an arrow to the line
#' @param cex_arrow arrow size
#' @param adj adjust the text position in x and y directions
#'
#' @return No return value, an text is displayed.
#' @export
#'
#' @examples
#' library(mapsf)
#' mf_theme("base")
#' mtq <- mf_get_mtq()
#' mtq_p <- mf_get_mtq("points")
#' mf_map(mtq, expandBB = c(0, .1, 0, .3))
#' mf_map(mtq_p[c(2, 3, 17, 28), ],
#'   pch = 4, lwd = 1.5, cex = .5, col = "red",
#'   add = TRUE
#' )
#' mf_title("Title of the map", banner = TRUE)
#' mf_frame()
#' mf_text(x = "topright", txt = "x = 'topright'")
#' mf_text(
#'   x = "bottomleft",
#'   txt = "x = 'bottomleft'\nadj = c(4,4)\nalign = 'left'",
#'   adj = c(4, 4),
#'   align = "left"
#' )
#' mf_text(
#'   x = c(728000, 1625500),
#'   txt = "x = c(X, Y)",
#'   pos = "right",
#'   line = 2,
#'   offset = 5
#' )
#' mf_text(
#'   mtq[3, ],
#'   txt = "pos = 'top'\nhalo = TRUE",
#'   pos = "top",
#'   align = "center",
#'   halo = TRUE
#' )
#' mf_text(
#'   mtq[3, ],
#'   txt = "pos = 'bottomleft'\nhalo = TRUE\nalign = 'right'",
#'   pos = "bottomleft",
#'   align = "right",
#'   halo = TRUE
#' )
#' mf_text(
#'   x = mtq[17, ],
#'   txt = "pos = 'bottomright'\nline = 1\nbox = TRUE",
#'   pos = "bottomright",
#'   offset = 10,
#'   line = 1,
#'   box = TRUE
#' )
#' mf_text(
#'   x = mtq[17, ],
#'   txt = "pos = 'topright'\nline = 1\nalign = 'left'",
#'   pos = "topright",
#'   offset = 15,
#'   align = "left",
#'   line = 1
#' )
#' mf_text(
#'   x = mtq[2, ],
#'   txt = "pos = 'topleft'\nline = 2\nbox = TRUE\nclockwise = TRUE",
#'   pos = "topleft",
#'   offset = 8,
#'   clockwise = TRUE,
#'   line = 2,
#'   box = TRUE
#' )
#' mf_text(
#'   x = mtq[2, ],
#'   txt = "pos = 'bottomleft'\nline = 2\nclockwise = FALSE",
#'   pos = "bottomleft",
#'   offset = 6,
#'   clockwise = FALSE,
#'   line = 2,
#'   box = FALSE
#' )
#' mf_text(
#'   x = mtq[28, ],
#'   txt = "pos = 'topright'\nline = 3\nclockwise = FALSE",
#'   pos = "topright",
#'   offset = 10,
#'   clockwise = FALSE,
#'   line = 3,
#'   halo = TRUE,
#'   align = "left"
#' )
#' mf_text(
#'   x = mtq[28, ],
#'   txt = "pos = 'right'\nline = 3\nbox = TRUE\nclockwise = TRUE",
#'   pos = "right",
#'   offset = 10,
#'   clockwise = TRUE,
#'   line = 3,
#'   box = TRUE
#' )
mf_text <- function(
  x,
  txt = "Text",
  cex = .8,
  col_txt,
  pos = "center",
  offset = 0,
  align = "center",
  font = 1,
  family = "sans",
  halo = FALSE,
  col_halo,
  cex_halo = cex,
  box = FALSE,
  col_box,
  col_box_border,
  lwd_box = 2,
  line = 0,
  clockwise = TRUE,
  lwd = 2,
  col_line,
  arrow = TRUE,
  cex_arrow = 1,
  adj = c(0, 0)
) {
  test_cur_plot()
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))

  if (inherits(x, "character") && x == "interactive") {
    x <- interleg(txt = c("text", "Text"))
  }

  col_txt <- go(col_txt, "highlight")
  col_halo <- go(col_halo, "background")
  col_box <- go(col_box, "background")
  col_box_border <- go(col_box_border, "highlight")
  col_line <- go(col_line, "highlight")

  recordGraphics(
    expr = mf_text_display(
      x = x, txt = txt, cex = cex, col_txt = col_txt,
      pos = pos, offset = offset, align = align,
      font = font, family = family,
      halo = halo, col_halo = col_halo,
      cex_halo = cex_halo,
      box = box, col_box = col_box,
      col_box_border = col_box_border,
      lwd_box = lwd_box, line = line,
      clockwise = clockwise,
      lwd = lwd, col_line = col_line, arrow = arrow,
      cex_arrow = cex_arrow,
      adj = adj
    ),
    list = list(
      x = x, txt = txt, cex = cex, col_txt = col_txt,
      pos = pos, offset = offset, align = align, font = font,
      family = family,
      halo = halo, col_halo = col_halo, cex_halo = cex_halo,
      box = box, col_box = col_box, col_box_border = col_box_border,
      lwd_box = lwd_box, line = line, clockwise = clockwise,
      lwd = lwd, col_line = col_line, arrow = arrow,
      cex_arrow = cex_arrow,
      adj = adj
    ),
    env = getNamespace("mapsf")
  )


  return(invisible(NULL))
}


mf_text_display <- function(
  x,
  txt = "Text",
  cex = .8,
  col_txt,
  pos = "center",
  offset = .5,
  align = "center",
  font = 1,
  family = "sans",
  halo = FALSE,
  col_halo,
  cex_halo = cex,
  box = FALSE,
  col_box,
  col_box_border,
  lwd_box = 2,
  line = 0,
  clockwise = TRUE,
  lwd = 2,
  col_line,
  arrow = TRUE,
  cex_arrow = 1,
  adj = c(0, 0)
) {
  res <- get_xy_for_txt(x, adj = adj, pos = pos, offset = offset)
  x <- res$x
  y <- res$y
  pos <- res$pos
  offset <- res$offset

  # internal var
  offset_x <- xinch(par("csi")) / 2
  offset_y <- yinch(par("csi")) / 2
  wtext <- strwidth(txt, units = "user", cex = cex, font = font, family = family)
  htext <- strheight(txt, units = "user", cex = cex, font = font, family = family)
  pos <- get_position_from_name(pos)
  xy <- xy_pos(
    x = x, y = y,
    cex = offset / 2,
    offset_x = offset_x,
    offset_y = offset_y,
    pos = pos
  )
  if (pos == 0) {
    adj_y <- .5
    xy <- c(x, y)
    if (align == "right") {
      adj_x <- 1
      xy_text <- c(x + wtext / 2, y)
    }
    if (align == "left") {
      adj_x <- 0
      xy_text <- c(x - wtext / 2, y)
    }
    if (align == "center") {
      adj_x <- .5
      xy_text <- c(x, y)
    }
    box_bbox <- list(
      xleft   = xy[1] - (wtext / 2 + offset_x / 2),
      ybottom = xy[2] - (htext / 2 + offset_y / 2),
      xright  = xy[1] + (wtext / 2 + offset_x / 2),
      ytop    = xy[2] + (htext / 2 + offset_y / 2)
    )
  }

  if (pos == 1) {
    adj_y <- 1
    if (align == "right") {
      adj_x <- 1
      xy_text <- c(xy[1] + wtext / 2, xy[2])
    }
    if (align == "left") {
      adj_x <- 0
      xy_text <- c(xy[1] - wtext / 2, xy[2])
    }
    if (align == "center") {
      adj_x <- .5
      xy_text <- c(xy[1], xy[2])
    }
    xy_text[2] <- xy_text[2] - offset_y / 2
    box_bbox <- list(
      xleft   = xy[1] - (wtext / 2 + offset_x / 2),
      ybottom = xy[2] - (htext + offset_y),
      xright  = xy[1] + (wtext / 2 + offset_x / 2),
      ytop    = xy[2]
    )
  }

  if (pos == 2) {
    adj_y <- 1
    if (align == "right") {
      adj_x <- 1
      xy_text <- c(xy[1], xy[2])
    }
    if (align == "left") {
      adj_x <- 0
      xy_text <- c(xy[1] - wtext, xy[2])
    }
    if (align == "center") {
      adj_x <- .5
      xy_text <- c(xy[1] - wtext / 2, xy[2])
    }
    xy_text[1] <- xy_text[1] - offset_x / 2
    xy_text[2] <- xy_text[2] - offset_y / 2
    box_bbox <- list(
      xleft   = xy[1] - (wtext + offset_x),
      ybottom = xy[2] - (htext + offset_y),
      xright  = xy[1],
      ytop    = xy[2]
    )
  }

  if (pos == 3) {
    adj_y <- 0.5
    if (align == "right") {
      adj_x <- 1
      xy_text <- c(xy[1], xy[2])
    }
    if (align == "left") {
      adj_x <- 0
      xy_text <- c(xy[1] - wtext, xy[2])
    }
    if (align == "center") {
      adj_x <- .5
      xy_text <- c(xy[1] - wtext / 2, xy[2])
    }
    xy_text[1] <- xy_text[1] - offset_x / 2
    box_bbox <- list(
      xleft   = xy[1] - (wtext + offset_x),
      ybottom = xy[2] - (htext / 2 + offset_y / 2),
      xright  = xy[1],
      ytop    = xy[2] + (htext / 2 + offset_y / 2)
    )
  }

  if (pos == 4) {
    adj_y <- 0
    if (align == "right") {
      adj_x <- 1
      xy_text <- c(xy[1], xy[2])
    }
    if (align == "left") {
      adj_x <- 0
      xy_text <- c(xy[1] - wtext, xy[2])
    }
    if (align == "center") {
      adj_x <- .5
      xy_text <- c(xy[1] - wtext / 2, xy[2])
    }
    xy_text[1] <- xy_text[1] - offset_x / 2
    xy_text[2] <- xy_text[2] + offset_y / 2
    box_bbox <- list(
      xleft   = xy[1] - (wtext + offset_x),
      ybottom = xy[2],
      xright  = xy[1],
      ytop    = xy[2] + (htext + offset_y)
    )
  }

  if (pos == 5) {
    adj_y <- 0
    if (align == "right") {
      adj_x <- 1
      xy_text <- c(xy[1] + wtext / 2, xy[2])
    }
    if (align == "left") {
      adj_x <- 0
      xy_text <- c(xy[1] - wtext / 2, xy[2])
    }
    if (align == "center") {
      adj_x <- .5
      xy_text <- c(xy[1], xy[2])
    }
    xy_text[2] <- xy_text[2] + offset_y / 2
    box_bbox <- list(
      xleft   = xy[1] - (wtext / 2 + offset_x / 2),
      ybottom = xy[2],
      xright  = xy[1] + (wtext / 2 + offset_x / 2),
      ytop    = xy[2] + (htext + offset_y)
    )
  }

  if (pos == 6) {
    adj_y <- 0
    if (align == "right") {
      adj_x <- 1
      xy_text <- c(xy[1] + wtext, xy[2])
    }
    if (align == "left") {
      adj_x <- 0
      xy_text <- c(xy[1], xy[2])
    }
    if (align == "center") {
      adj_x <- .5
      xy_text <- c(xy[1] + wtext / 2, xy[2])
    }
    xy_text[1] <- xy_text[1] + offset_x / 2
    xy_text[2] <- xy_text[2] + offset_y / 2
    box_bbox <- list(
      xleft   = xy[1],
      ybottom = xy[2],
      xright  = xy[1] + (wtext + offset_x),
      ytop    = xy[2] + (htext + offset_y)
    )
  }

  if (pos == 7) {
    adj_y <- 0.5
    if (align == "right") {
      adj_x <- 1
      xy_text <- c(xy[1] + wtext, xy[2])
    }
    if (align == "left") {
      adj_x <- 0
      xy_text <- c(xy[1], xy[2])
    }
    if (align == "center") {
      adj_x <- .5
      xy_text <- c(xy[1] + wtext / 2, xy[2])
    }
    xy_text[1] <- xy_text[1] + offset_x / 2
    box_bbox <- list(
      xleft   = xy[1],
      ybottom = xy[2] - (htext / 2 + offset_y / 2),
      xright  = xy[1] + (wtext + offset_x),
      ytop    = xy[2] + (htext / 2 + offset_y / 2)
    )
  }

  if (pos == 8) {
    adj_y <- 1
    if (align == "right") {
      adj_x <- 1
      xy_text <- c(xy[1] + wtext, xy[2])
    }
    if (align == "left") {
      adj_x <- 0
      xy_text <- c(xy[1], xy[2])
    }
    if (align == "center") {
      adj_x <- .5
      xy_text <- c(xy[1] + wtext / 2, xy[2])
    }
    xy_text[1] <- xy_text[1] + offset_x / 2
    xy_text[2] <- xy_text[2] - offset_y / 2
    box_bbox <- list(
      xleft   = xy[1],
      ybottom = xy[2] - (htext + offset_y),
      xright  = xy[1] + (wtext + offset_x),
      ytop    = xy[2]
    )
  }


  display_line(
    line = line, pos = pos, box = box, x = x, y = y, xy = xy,
    offset_x = offset_x, offset_y = offset_y, lwd = lwd,
    col_line = col_line, arrow = arrow, cex_arrow, offset = offset,
    htext = htext, wtext = wtext, clockwise = clockwise
  )

  display_text(
    x = xy_text[1], y = xy_text[2], txt = txt, col_txt = col_txt,
    cex = cex, font = font, family = family, adj = c(adj_x, adj_y),
    offset_x = offset_x, offset_y = offset_y,
    box = box, box_bbox = box_bbox, col_box = col_box,
    col_box_border = col_box_border, lwd_box = lwd_box, halo = halo,
    cex_halo = cex_halo, col_halo = col_halo
  )
}
