prop <- function(x,
                 var,
                 inches = 0.3,
                 val_max,
                 lwd_max = 20,
                 symbol = "circle",
                 col,
                 alpha = NULL,
                 border,
                 lwd = .7,
                 extent = x,
                 bg,
                 expandBB = rep(0, 4),
                 leg_pos = "top",
                 leg_title = var,
                 leg_title_cex = .8,
                 leg_val_cex = .6,
                 leg_val_rnd = 0,
                 leg_val_dec = ".",
                 leg_val_big = "",
                 leg_frame = FALSE,
                 leg_frame_border,
                 leg_horiz = FALSE,
                 leg_adj = c(0, 0),
                 leg_fg,
                 leg_bg,
                 leg_size = 1,
                 add,
                 geom_type) {
  # default
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op), add = TRUE)
  border <- border[[1]]
  col <- mapsf:::get_hex_pal(col, alpha)
  x <- prop_data_cleaning(x, var)
  val_max <- get_val_max(x, var, val_max)

    # empty plot if needed
  if (add == FALSE) {
    mf_ini(x, expandBB = expandBB, extent = extent, bgc = bg)
  }

  if (geom_type == "LINE") {
    op_line <- par(lend = 1)
    on.exit(par(op_line), add = TRUE)

    x$lwd <- x[[var]] * lwd_max / val_max
    mf_map(x, lwd = x$lwd, add = TRUE, col = col)
    val <- seq(min(x[[var]]), max(x[[var]]), length.out = 4)
    leg(
      type = "prop_line",
      pos = leg_pos,
      val = val,
      lwd = max(xl$lwd),
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
    return(invisible(NULL))
  }

  # check merge and order
  dots <- cbind(st_coordinates(x),st_set_geometry(x, NULL))

  # get sizes
  sizes <- get_size(
    var = dots[[var]], inches = inches,
    val_max = val_max, symbol = symbol
  )

  # size and values for legend, hollow circle (fixmax case)
  size_max <- max(sizes)
  val <- seq(sqrt(min(dots[[var]])), sqrt(max(dots[[var]])), length.out = 4)
  val <- val * val

  mycols <- rep(col, nrow(dots))

  if (inches <= size_max) {
    inches <- size_max
    borders <- border
  } else {
    mycols <- c(NA, mycols)
    borders <- c(NA, rep(border, nrow(dots)))
    dots <- rbind(dots[1, ], dots)
    dots[1, var] <- val_max
    sizes <- c(inches, sizes)
  }

  # Plot the symbols
  plot_symbols(
    symbol = symbol, dots = dots, sizes = xinch(sizes),
    mycols = mycols, border = borders, lwd = lwd,
    inches = inches
  )

  # symbols size
  leg(
    type = "prop",
    pos = leg_pos,
    val = val,
    title = leg_title,
    symbol = symbol,
    inches = size_max,
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

  return(invisible(NULL))
}
