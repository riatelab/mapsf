base <- function(x,
                 col,
                 border,
                 alpha,
                 cex,
                 pch,
                 lwd,
                 lty,
                 add,
                 extent,
                 bg,
                 expandBB,
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
    cols <- get_point_cols(pch, col, border)
    plot(
      st_geometry(x),
      col = cols$col, bg = cols$bg, cex = cex, pch = pch,
      lwd = lwd, add = TRUE
    )
  }

  return(invisible(x))
}



