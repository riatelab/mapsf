#' @title Plot credits
#' @description Plot credits (sources, author, year...).
#' @name mf_credits
#' @eval my_params(c('col'))
#' @param pos position, one of 'bottomleft', 'bottomright' or 'rightbottom'
#' @param txt text of the credits, use '\\n' to add line breaks
#' @param cex cex of the credits
#' @param font font of the credits
#' @param bg background color
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
  if (missing(col)) {
    col <- getOption("mapsf.fg")
  }
  pd <- par("usr")
  pdp <- xinch(par("csi")) / 4

  w <- strwidth(s = txt, units = "user", cex = cex, font = font)
  h <- strheight(s = txt, units = "user", cex = cex, font = font)

  ppos <- switch(pos,
    bottomleft = {
      xr <- pd[1] + w + 2 * pdp
      xl <- pd[1]
      yb <- pd[3]
      yt <- pd[3] + h + 2 * pdp
      pd[1] <- pd[1] + pdp
      pd[3] <- pd[3] + pdp
      list(
        pd = pd, adj = c(0, 0), srt = 0,
        xr = xr, xl = xl, yb = yb, yt = yt
      )
    },
    bottomright = {
      xr <- pd[2]
      xl <- pd[2] - w - 2 * pdp
      yb <- pd[3]
      yt <- pd[3] + h + 2 * pdp
      pd[1] <- pd[2] - pdp
      pd[3] <- pd[3] + pdp
      list(
        pd = pd, adj = c(1, 0), srt = 0,
        xr = xr, xl = xl, yb = yb, yt = yt
      )
    },
    rightbottom = {
      xr <- pd[2]
      xl <- pd[2] - h - 2 * pdp
      yb <- pd[3]
      yt <- pd[3] + w + 2 * pdp
      pd[1] <- pd[2] - pdp
      pd[3] <- pd[3] + pdp
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
