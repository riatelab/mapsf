#' @title Plot a title
#' @param txt title text
#' @param pos position, one of 'left', 'center', 'right'
#' @param tab if TRUE the title is displayed as a tab
#' @param bg background of the title
#' @param fg foreground of the title
#' @param cex cex of the title
#' @param font font of the title
#' @param line number of lines used for the title
#' @param inner if TRUE the title is displayed inside the plot area
#' @param banner if TRUE the title is dispalayed as a banner
#' @export
#' @importFrom grDevices recordGraphics
#' @return No return value, a title is displayed.
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_title()
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
  fg <- go(fg, "highlight", getOption("mapsf.background"))
  banner <- go(banner, "title_banner")

  if (isFALSE(banner)) {
    bg <- go(bg, opt = "background", legacy = bg)
  } else {
    bg <- go(bg, "foreground")
  }

  # correct line space for multiplot
  mmf <- par("mfrow")
  if (mmf[1] == 2 && mmf[2] == 2) {
    line <- line * 0.83
  }
  if (max(mmf) > 2) {
    line <- line * .66
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
  hbox <- line * 0.2 * xinch(1)
  inset <- xinch(par("csi")) / 4
  wtitle <- strwidth(txt, units = "user", cex = cex, font = font)

  # compute rect coord
  pw <- pu[2] - pu[1]
  pb <- pu[c(1, 3, 2, 4)]
  pb <- switch(pos,
    left = {
      pb[3] <- pu[1] + inset + wtitle + inset
      pb
    },
    right = {
      pb[1] <- pu[2] - (inset + wtitle + inset)
      pb[3] <- pu[2]
      pb
    },
    center = {
      pb[1] <- pu[1] + (pw / 2) - (wtitle / 2) - inset
      pb[3] <- pb[1] + wtitle + inset + inset
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
  pt <- c(pb[1] + inset, pb[2] + (hbox) / 2)

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
