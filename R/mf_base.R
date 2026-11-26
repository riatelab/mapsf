base <- function(x,
                 col,
                 border,
                 alpha = NULL,
                 cex = 1,
                 pch = 20,
                 lwd = .7,
                 lty = 1,
                 add,
                 extent,
                 bg,
                 expandBB = rep(0, 4),
                 geom_type) {
  # margins mgmt
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  col <- get_hex_pal(col, alpha)

  if (add == FALSE) {
    mf_ini(x, expandBB = expandBB, extent = extent, bgc = bg)
  }


  if (geom_type == "LINE") {
    plot(
      st_geometry(x),
      col = col, lwd = lwd, lty = lty,
      add = TRUE
    )
  }

  if (geom_type == "POLYGON") {
    plot(
      st_geometry(x),
      col = col, border = border, lwd = lwd, lty = lty,
      add = TRUE
    )
  }

  if (geom_type == "POINT") {
    if (pch %in% 21:25) {
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
