choro <- function(x, var,
                  pal,
                  alpha,
                  rev,
                  breaks,
                  nbreaks,
                  border,
                  pch,
                  cex,
                  lwd,
                  col_na,
                  extent,
                  bg,
                  expandBB,
                  leg_pos,
                  leg_title,
                  leg_title_cex,
                  leg_val_cex,
                  leg_val_rnd,
                  leg_val_dec,
                  leg_val_big,
                  leg_no_data,
                  leg_frame,
                  leg_horiz,
                  leg_adj,
                  leg_size,
                  leg_box_border,
                  leg_box_cex,
                  leg_fg,
                  leg_bg,
                  leg_frame_border,
                  add,
                  geom_type) {
  # default
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op), add = TRUE)

  # get the breaks
  breaks <- mf_get_breaks(x = x[[var]], nbreaks = nbreaks, breaks = breaks)
  nbreaks <- length(breaks) - 1
  # get the color palette
  pal <- get_the_pal(pal = pal, nbreaks = nbreaks, alpha = alpha, rev = !rev)
  # get the color vector
  cols <- get_col_vec(x = x[[var]], breaks = breaks, pal = pal)
  ## NA mgmt
  no_data <- ifelse(any(is.na(cols)), TRUE, FALSE)
  cols[is.na(cols)] <- col_na


  if (add == FALSE) {
    mf_ini(x, expandBB = expandBB, extent = extent, bgc = bg)
  }

  if (geom_type == "LINE") {
    plot(st_geometry(x), col = cols, lwd = lwd, add = TRUE)
    leg(
      type = "choro_line",
      pos = leg_pos,
      val = breaks,
      title = leg_title,
      title_cex = leg_title_cex,
      val_cex = leg_val_cex,
      val_rnd = leg_val_rnd,
      val_dec = leg_val_dec,
      val_big = leg_val_big,
      col_na = col_na,
      no_data = no_data,
      no_data_txt = leg_no_data,
      frame = leg_frame,
      pal = pal,
      bg = leg_bg,
      fg = leg_fg,
      size = leg_size,
      lwd = lwd,
      box_cex = leg_box_cex,
      frame_border = leg_frame_border,
      adj = leg_adj
    )
  }

  if (geom_type == "POLYGON") {
    plot(
      st_geometry(x),
      col = cols, border = border, lwd = lwd,
      add = TRUE
    )
    leg(
      type = "choro",
      pos = leg_pos,
      val = breaks,
      title = leg_title,
      title_cex = leg_title_cex,
      val_cex = leg_val_cex,
      val_rnd = leg_val_rnd,
      val_dec = leg_val_dec,
      val_big = leg_val_big,
      col_na = col_na,
      no_data = no_data,
      no_data_txt = leg_no_data,
      horiz = leg_horiz,
      frame = leg_frame,
      pal = pal,
      bg = leg_bg,
      fg = leg_fg,
      size = leg_size,
      box_border = leg_box_border,
      box_cex = leg_box_cex,
      frame_border = leg_frame_border,
      adj = leg_adj
    )
  }

  if (geom_type == "POINT") {
    cols <- get_point_cols(pch, cols, border)
    plot(
      st_geometry(x),
      col = cols$col, bg = cols$bg, cex = cex, pch = pch,
      lwd = lwd, add = TRUE
    )
    leg(
      type = "choro_symb",
      pos = leg_pos,
      val = breaks,
      pch = pch,
      cex = cex,
      lwd = lwd,
      border = border,
      title = leg_title,
      title_cex = leg_title_cex,
      val_cex = leg_val_cex,
      val_rnd = leg_val_rnd,
      val_dec = leg_val_dec,
      val_big = leg_val_big,
      col_na = col_na,
      no_data = no_data,
      no_data_txt = leg_no_data,
      horiz = leg_horiz,
      frame = leg_frame,
      pal = pal,
      bg = leg_bg,
      fg = leg_fg,
      size = leg_size,
      box_cex = leg_box_cex,
      frame_border = leg_frame_border,
      adj = leg_adj
    )
  }

  return(invisible(x))
}
