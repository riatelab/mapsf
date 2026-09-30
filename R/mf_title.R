#' @title Plot a title
#'
#' @description
#' Default values for `mf_title()` arguments are set with the map theme
#' (see [mf_theme]).
#'
#' It is possible to display a title and a subtitle with a combinaison of
#' [mf_theme] parameters and [mf_text] calls, see in examples.
#'
#' @param txt title text
#' @param pos position, one of 'left', 'center', 'right'
#' @param tab if TRUE the title is displayed as a tab
#' @param bg background color of the title, hex code or color name given by
#' [colors]
#' @param fg foreground color of the title, hex code or color name given by
#' [colors]
#' @param cex size of the title
#' @param font font of the title
#' @param line number of margin lines used for the title
#' @param inner if TRUE the title is displayed inside the plot area;
#' if FALSE the title is displayed in the top margin
#' @param banner if TRUE the title is displayed as a banner
#' @export
#' @return No return value, a title is displayed.
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_title()
#'
#' # Main and sub titles with mf_theme() params and mf_text() calls
#' mf_theme(x = "base", mar = c(.5, .5, .5 + 3, .5), title_line = 3,
#'          title_banner = TRUE)
#' mf_map(m)
#' mf_title(txt = "")
#' mf_text(x = "top", txt = "Main Title", adj = c(0, 11),
#'         cex = 1.5, font = 2,  xpd = TRUE)
#' mf_text(x = "top", txt = "Sub Title", adj = c(0, 5),
#'         cex = 1, font = 3, xpd = TRUE)
#' mf_theme(NULL)
mf_title <- function(txt = "Map Title", pos, tab,
                     bg, fg, cex, line, font,
                     inner, banner) {
  test_cur_plot()
  tab <- go(tab, "title_tab")
  pos <- go(pos, "title_pos")
  inner <- go(inner, "title_inner")
  line <- go(line, "title_line")
  cex <- go(cex, "title_cex")
  font <- go(font, "title_font")
  fg <- go(fg, "highlight")
  banner <- go(banner, "title_banner")

  if (isFALSE(banner)) {
    bg <- go(bg, "background")
  } else {
    bg <- go(bg, "foreground")
  }

  recordGraphics(
    {
      mf_title_display(txt, pos, tab, bg, fg, cex, line, font, inner, banner)
    },
    list = list(
      txt = txt,
      pos = pos,
      tab = tab,
      bg = bg,
      fg = fg,
      cex = cex,
      line = line,
      font = font,
      inner = inner,
      banner = banner
    ),
    env = getNamespace("mapsf")
  )
}

mf_title_display <- function(txt = "Map Title", pos, tab,
                             bg, fg, cex, line, font,
                             inner, banner) {
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))
  # size refs
  pu <- par("usr")
  hbox <- line * par("csi") * yinch(1)
  inset_x <- xinch(par("csi")) / 4

  wtitle <- strwidth(txt, units = "user", cex = cex, font = font)

  # compute rect coord
  pw <- pu[2] - pu[1]
  pb <- pu[c(1, 3, 2, 4)]
  pb <- switch(pos,
    left = {
      pb[3] <- pu[1] + inset_x + wtitle + inset_x
      pb
    },
    right = {
      pb[1] <- pu[2] - (inset_x + wtitle + inset_x)
      pb[3] <- pu[2]
      pb
    },
    center = {
      pb[1] <- pu[1] + (pw / 2) - (wtitle / 2) - inset_x
      pb[3] <- pb[1] + wtitle + inset_x + inset_x
      pb
    }
  )
  # adjust box coord if inner
  if (inner) {
    pb[2] <- pu[4] - hbox
    pb[4] <- pu[4]
  } else {
    pb[2] <- pu[4]
    pb[4] <- pu[4] + hbox
  }
  # title coord
  pt <- c(pb[1] + inset_x, pb[2] + (hbox) / 2)

  # adjust box
  if (tab == FALSE) {
    pb[c(1, 3)] <- pu[1:2]
  }

  # display rect
  rect(
    xleft = pb[1],
    ybottom = pb[2] - hbox * 0.02,
    xright = pb[3],
    ytop = pb[4],
    col = bg,
    xpd = TRUE,
    border = NA
  )

  # display title
  text(
    x = pt[1],
    y = pt[2],
    labels = txt, adj = c(0, 0.5),
    cex = cex, col = fg,
    font = font, xpd = TRUE
  )
  f <- getOption("mapsf.frame")
  if (f %in% c("map", "figure")) {
    mf_frame(
      extent = f,
      col = getOption("mapsf.highlight"),
      lwd = getOption("mapsf.frame_lwd"),
      lty = getOption("mapsf.frame_lty")
    )
  }

  return(invisible(NULL))
}
