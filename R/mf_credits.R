#' @title Plot credits
#' @description Plot credits (sources, author, year...).
#' @name mf_credits
#' @param col color of the text, hex code or color name given by [colors].
#' The default color is the highlight color (see [mf_theme]).
#' @param pos position, one of 'bottomleft', 'bottomright' or 'rightbottom'
#' @param txt text of the credits, use '\\n' to add line breaks
#' @param cex size of the text
#' @param font font of the text
#' @param bg background color, hex code or color name given by [colors]
#' @export
#' @return No return value, credits are displayed.
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_credits(txt = "Author\nSources - Year")
mf_credits <- function(txt = "Source(s) & Author(s)",
                       pos = "bottomleft",
                       col,
                       cex = .6,
                       font = 3,
                       bg = NA) {
  test_cur_plot()

  col <- go(col, "highlight")


  recordGraphics(
    {
      mf_credits_display(txt, pos, col, cex, font, bg)
    },
    list = list(
      txt = txt,
      pos = pos,
      col = col,
      cex = cex,
      font = font,
      bg = bg
    ),
    env = getNamespace("mapsf")
  )
}


mf_credits_display <- function(txt = "Source(s) & Author(s)",
                               pos = "bottomleft",
                               col,
                               cex = .6,
                               font = 3,
                               bg = NA) {
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))
  xyi <- xyinch(1)
  ratio <- xyi[2] / xyi[1]


  pd <- par("usr")
  pdp_x <- xinch(par("csi")) / 4
  pdp_y <- yinch(par("csi")) / 4


  w <- strwidth(s = txt, units = "user", cex = cex, font = font)
  h <- strheight(s = txt, units = "user", cex = cex, font = font)

  ppos <- switch(pos,
    bottomleft = {
      xr <- pd[1] + w + 2 * pdp_x
      xl <- pd[1]
      yb <- pd[3]
      yt <- pd[3] + h + 2 * pdp_y
      pd[1] <- pd[1] + pdp_x
      pd[3] <- pd[3] + pdp_y
      list(
        pd = pd, adj = c(0, 0), srt = 0,
        xr = xr, xl = xl, yb = yb, yt = yt
      )
    },
    bottomright = {
      xr <- pd[2]
      xl <- pd[2] - w - 2 * pdp_x
      yb <- pd[3]
      yt <- pd[3] + h + 2 * pdp_y
      pd[1] <- pd[2] - pdp_x
      pd[3] <- pd[3] + pdp_y
      list(
        pd = pd, adj = c(1, 0), srt = 0,
        xr = xr, xl = xl, yb = yb, yt = yt
      )
    },
    rightbottom = {
      xr <- pd[2]
      xl <- pd[2] - h / ratio - 2 * pdp_x
      yb <- pd[3]
      yt <- pd[3] + w * ratio + 2 * pdp_y
      pd[1] <- pd[2] - pdp_x
      pd[3] <- pd[3] + pdp_y
      list(
        pd = pd, adj = c(0, 0), srt = 90,
        xr = xr, xl = xl, yb = yb, yt = yt
      )
    }
  )


  rect(
    xleft = ppos$xl, ybottom = ppos$yb, xright = ppos$xr,
    ytop = ppos$yt, col = bg, border = NA, xpd = TRUE
  )

  text(
    x = ppos$pd[1], y = ppos$pd[3], labels = txt,
    cex = cex, xpd = TRUE, adj = ppos$adj,
    col = col, srt = ppos$srt, font = font
  )
}
