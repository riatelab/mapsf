prop_choro <- function(x,
                          var,
                          inches = 0.3,
                          val_max,
                          symbol = "circle",
                          pal,
                          alpha = NULL,
                          rev = FALSE,
                          breaks = "quantile",
                          nbreaks,
                          border,
                          lwd = .7,
                          col_na = "white",
                          extent = x,
                          bg,
                          expandBB = rep(0, 4),
                          leg_pos = "top",
                          leg_title = var,
                          leg_title_cex = .8,
                          leg_val_cex = .6,
                          leg_val_rnd = c(0, 2),
                          leg_val_dec = ".",
                          leg_val_big = "",
                          leg_no_data = "No data",
                          leg_frame = FALSE,
                          leg_frame_border,
                          leg_horiz = c(FALSE, FALSE),
                          leg_adj = c(0, 0),
                          leg_fg,
                          leg_bg,
                          leg_size = 1,
                          leg_box_border,
                          leg_box_cex = c(1, 1),
                          add = FALSE) {
  xtype <- get_geom_type(x)
  # linestring special case
  if (xtype == "LINE") {
    message("This map type is not available for lines.")
    return(invisible(NULL))
  }

  # default
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))

  bgc <- go(bg, "background")
  pal <- go(pal, "pal_seq")
  leg_box_border <- go(leg_box_border, "highlight")
  leg_fg <- go(leg_fg, "highlight")
  leg_bg <- go(leg_bg, "foreground")
  leg_frame_border <- go(leg_frame_border, "foreground")
  border <- go(border, "background")
  border <- border[[1]]

  var2 <- var[2]
  var1 <- var[1]
  # check merge and order
  dots <- create_dots(x = x, var = var1)

  # jenks
  jen <- FALSE
  if (any(breaks %in% "jenks")) {
    jen <- TRUE
  }
  # get the breaks
  breaks <- mf_get_breaks(
    x = dots[[var2]], nbreaks = nbreaks,
    breaks = breaks
  )
  nbreaks <- length(breaks) - 1
  # get the cols
  pal <- get_the_pal(pal = pal, nbreaks = nbreaks, alpha = alpha, rev = !rev)
  # get the color vector
  mycols <- get_col_vec(x = dots[[var2]], breaks = breaks, pal = pal, jen = jen)

  no_data <- FALSE
  if (max(is.na(mycols)) == 1) {
    no_data <- TRUE
  }
  mycols[is.na(mycols)] <- col_na

  # Default max value
  if (missing(val_max)) {
    val_max <- max(dots[[var1]])
  }

  # get sizes
  sizes <- get_size(
    var = dots[[var1]], inches = inches,
    val_max = val_max, symbol = symbol
  )

  # size and values for legend, hollow circle (fixmax case)
  size_max <- max(sizes)
  val <- seq(sqrt(min(dots[[var1]])), sqrt(max(dots[[var1]])), length.out = 4)
  val <- val * val
  if (inches <= size_max) {
    inches <- size_max
    borders <- border
  } else {
    mycols <- c(NA, mycols)
    borders <- c(NA, rep(border, nrow(dots)))
    dots <- rbind(dots[1, ], dots)
    dots[1, var1] <- val_max
    sizes <- c(inches, sizes)
  }

  # empty plot if needed
  if (add == FALSE) {
    mf_ini(x, expandBB = expandBB, extent = extent, bgc = bgc)
  }

  # Plot the symbols
  plot_symbols(
    symbol = symbol, dots = dots, sizes = xinch(sizes),
    mycols = mycols, border = borders, lwd = lwd,
    inches = inches
  )

  leg_pos <- defunct_leg(leg_pos)

  if (is.null(getOption("mapsf.legacy"))) {
    ccol <- getOption("mapsf.foreground")
  } else {
    ccol <- "grey80"
  }
  if (all(leg_frame, !leg_horiz)) {
    ccol <- getOption("mapsf.foreground")
  }

  leg_title <- test2args(leg_title)
  leg_val_rnd <- test2args(leg_val_rnd)
  leg_horiz <- test2args(leg_horiz)

  la1 <- list(
    type = "prop",
    val = val,
    title = leg_title[1],
    symbol = symbol,
    inches = size_max,
    col = ccol,
    val_rnd = leg_val_rnd[1],
    val_dec = leg_val_dec,
    val_big = leg_val_big,
    border = border,
    lwd = lwd,
    horiz = leg_horiz[1],
    self_adjust = TRUE
  )
  lg <- do.call(leg_comp, la1)
  la2 <- list(
    leg = lg,
    type = "choro_point",
    symbol = symbol,
    val = breaks,
    title = leg_title[2],
    val_rnd = leg_val_rnd[2],
    val_dec = leg_val_dec,
    val_big = leg_val_big,
    col_na = col_na,
    no_data = no_data,
    no_data_txt = leg_no_data,
    horiz = leg_horiz[2],
    pal = pal,
    border = border
  )
  lg <- do.call(leg_comp, la2)
  leg_draw(lg,
           pos = leg_pos, bg = leg_bg, fg = leg_fg, size = leg_size,
           frame = leg_frame, title_cex = leg_title_cex,
           val_cex = leg_val_cex, mar = getOption("mapsf.mar"),
           adj = leg_adj, frame_border = leg_frame_border
  )
  return(invisible(x))
}
