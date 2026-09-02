base <- function(x,
                 col,
                 border,
                 alpha = NULL,
                 cex = 1,
                 pch = 20,
                 lwd = .7,
                 lty = 1,
                 add = FALSE,
                 extent = x,
                 bg,
                 expandBB = rep(0, 4),
                 ...) {
  # margins mgmt
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))

  bgc <- go(bg, "background")

  if (add == FALSE) {
    mf_ini(x, expandBB = expandBB, extent = extent, bgc = bgc)
  }

  xtype <- get_geom_type(x)

  if (xtype == "LINE") {
    col <- go(col, "highlight")
    if (!is.null(alpha)) {
      col <- get_hex_pal(col, alpha)
    }
    plot(
      st_geometry(x),
      col = col, lwd = lwd, lty = lty,
      add = TRUE
    )
  }

  if (xtype == "POLYGON") {
    col <- go(col, "foreground")
    border <- go(border, "highlight")
    if (!is.null(alpha)) {
      col <- get_hex_pal(col, alpha)
    }
    plot(
      st_geometry(x),
      col = col, border = border, lwd = lwd, lty = lty,
      add = TRUE
    )
  }

  if (xtype == "POINT") {
    col <- go(col, "highlight")
    if (!is.null(alpha)) {
      col <- get_hex_pal(col, alpha)
    }
    if (pch %in% 21:25) {
      if (missing(border)) {
        border <- go(border, "foreground")
      }
      mycolspt <- border
    } else {
      mycolspt <- col
    }
    mycolsptbg <- col
    plot(
      st_geometry(x),
      col = mycolspt, bg = mycolsptbg, cex = cex, pch = pch,
      lwd = lwd, add = TRUE
    )
  }

  return(invisible(x))
}
