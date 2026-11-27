prop <- function(x,
                 var,
                 inches,
                 val_max,
                 lwd_max,
                 symbol,
                 col,
                 alpha,
                 border,
                 lwd,
                 extent = x,
                 bg,
                 expandBB,
                 leg_pos,
                 leg_title,
                 leg_title_cex,
                 leg_val_cex,
                 leg_val_rnd,
                 leg_val_dec,
                 leg_val_big,
                 leg_frame,
                 leg_frame_border,
                 leg_horiz,
                 leg_adj,
                 leg_fg,
                 leg_bg,
                 leg_size,
                 add,
                 geom_type) {
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op), add = TRUE)

  col <- get_hex_pal(col, alpha)
  x <- prop_data_cleaning(x, var)
  val_max <- get_val_max(x, var, val_max)

  if (add == FALSE) {
    mf_ini(x, expandBB = expandBB, extent = extent, bgc = bg)
  }

  if (geom_type == "LINE") {
    op_line <- par(lend = 1)
    on.exit(par(op_line), add = TRUE)

    x$lwd <- x[[var]] * lwd_max / val_max
    leg_values <- seq(min(x[[var]]), max(x[[var]]), length.out = 4)

    mf_map(x, lwd = x$lwd, add = TRUE, col = col)

    leg(
      type = "prop_line",
      pos = leg_pos,
      val = leg_values,
      lwd = max(x$lwd),
      col = col,
      title = leg_title,
      title_cex = leg_title_cex,
      val_cex = leg_val_cex,
      val_rnd = leg_val_rnd,
      val_dec = leg_val_dec,
      val_big = leg_val_big,
      frame = leg_frame,
      bg = leg_bg,
      fg = leg_fg,
      adj = leg_adj,
      frame_border = leg_frame_border,
      size = leg_size
    )
  }
  if (geom_type == "POINT") {
    x <- sf_to_coords(x)

    # get sizes in inches
    sizes <- get_size(
      var = x[[var]],
      inches = inches,
      val_max = val_max,
      symbol = symbol
    )

    # values for legend
    leg_values <- prop_leg_values(x[[var]])
    leg_size_max <- max(sizes)

    # adjust sizes for val_max & size_max use
    # by creating a larger transparent circle if needed
    pts <- adjust_size(
      x = x,
      var = var,
      sizes = sizes,
      border = border,
      col = col,
      inches = inches,
      val_max = val_max
    )

    # Plot the symbols
    plot_prop_symb(
      symbol = symbol,
      x = pts$x,
      lwd = lwd,
      inches = pts$inches
    )

    # Plot the legend
    leg(
      type = "prop",
      pos = leg_pos,
      val = leg_values,
      title = leg_title,
      symbol = symbol,
      inches = leg_size_max,
      col = col,
      title_cex = leg_title_cex,
      val_cex = leg_val_cex,
      val_rnd = leg_val_rnd,
      val_dec = leg_val_dec,
      val_big = leg_val_big,
      horiz = leg_horiz,
      frame = leg_frame,
      border = border,
      lwd = lwd,
      adj = leg_adj,
      bg = leg_bg,
      fg = leg_fg,
      self_adjust = TRUE,
      mar = getOption("mapsf.mar"),
      frame_border = leg_frame_border,
      size = leg_size
    )
  }
  return(invisible(NULL))
}
