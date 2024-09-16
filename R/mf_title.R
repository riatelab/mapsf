#' @title Plot a title
#' @param txt title text
#' @param pos position, one of 'left', 'center', 'right'
#' @param tab if TRUE the title is displayed as a 'tab'
#' @param bg background of the title
#' @param fg foreground of the title
#' @param cex cex of the title
#' @param font font of the title
#' @param line number of lines used for the title
#' @param inner if TRUE the title is displayed inside the plot area.
#' @export
#' @return No return value, a title is displayed.
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_title()
mf_title <- function(txt = "Map Title", pos, tab,
                     bg, fg, cex, line, font,
                     inner) {
  test_cur_plot()

  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))

  if (missing(tab)) tab <- getOption("mapsf.tab")
  if (missing(pos)) pos <- getOption("mapsf.pos")
  if (missing(inner)) inner <- getOption("mapsf.inner")
  if (missing(line)) line <- getOption("mapsf.line")
  if (missing(cex)) cex <- getOption("mapsf.cex")
  if (missing(font)) font <- getOption("mapsf.font")
  if (missing(bg)) bg <- getOption("mapsf.fg")
  if (missing(fg)) fg <- getOption("mapsf.bg")


  # correct line space for multiplot
  mmf <- par("mfrow")
  if (mmf[1] == 2 && mmf[2] == 2) {
    line <- line * 0.83
  }
  if (max(mmf) > 2) {
    line <- line * .66
  }

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
    ybottom = pb[2],
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


  return(invisible(NULL))
}
