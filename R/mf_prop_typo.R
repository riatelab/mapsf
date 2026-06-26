prop_typo <- function(x, var,
                         inches = 0.3,
                         val_max,
                         symbol = "circle",
                         pal,
                         alpha = NULL,
                         rev = FALSE,
                         val_order,
                         border,
                         lwd = .7,
                         lwd_max = 15,
                         col_na = "white",
                         extent = x,
                         bg,
                         expandBB = rep(0, 4),
                         leg_pos = mf_get_leg_pos(x, 1),
                         leg_title = var,
                         leg_title_cex = .8,
                         leg_val_cex = .6,
                         leg_val_rnd = 0,
                         leg_val_dec = ".",
                         leg_val_big = "",
                         leg_no_data = "No data",
                         leg_frame = FALSE,
                         leg_frame_border,
                         leg_horiz = FALSE,
                         leg_adj = c(0, 0),
                         leg_fg,
                         leg_bg,
                         leg_size = 1,
                         leg_box_border,
                         leg_box_cex = c(1, 1),
                         add = TRUE) {
  # default
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))

  bgc <- go(bg, "background")
  pal <- go(pal, "pal_quali", "Dynamic")
  leg_box_border <- go(leg_box_border, "highlight")
  leg_fg <- go(leg_fg, "highlight")
  leg_bg <- go(leg_bg, "foreground", getOption("mapsf.background"))
  leg_frame_border <- go(
    leg_frame_border, "foreground",
    getOption("mapsf.highlight")
  )
  border <- go(border, "background")
  border <- border[[1]]

  var2 <- var[2]
  var1 <- var[1]

  xtype <- get_geom_type(x)
  # linestring special case
  if (xtype == "LINE") {
    xl <- x[!is.na(x[[var1]]), ]
    if (!missing(val_max)) {
      maxval <- val_max
    } else {
      maxval <- max(xl[[var1]])
    }
    xl$lwd <- xl[[var1]] * lwd_max / maxval
    if (add == FALSE) {
      mf_ini(x, expandBB = expandBB, extent = extent, bgc = bgc)
    }
    xl <- xl[xl[[var1]] != 0, ]
    # turn to positive values
    xl[[var1]] <- abs(xl[[var1]])
    # Order the dots
    xl <- xl[order(xl[[var1]], decreasing = TRUE), ]

    val_order <- get_modalities(
      x = xl[[var2]],
      val_order = val_order
    )
    # get color list and association
    pal <- get_the_pal(
      pal = pal, nbreaks = length(val_order), alpha = alpha,
      rev = !rev
    )
    # get color vector
    mycols <- get_col_typo(
      x = xl[[var2]], pal = pal,
      val_order = val_order
    )

    no_data <- FALSE
    if (max(is.na(mycols)) == 1) {
      no_data <- TRUE
    }
    mycols[is.na(mycols)] <- col_na
    op2 <- par(lend = 1)
    on.exit(par(op2))
    mf_map(xl, lwd = xl$lwd, add = TRUE, col = mycols)
    val <- seq(min(xl[[var1]]), max(xl[[var1]]), length.out = 4)
    leg_pos <- defunct_leg(leg_pos)

    ccol <- getOption("mapsf.highlight")

    la1 <- list(
      type = "prop_line",
      val = val,
      title = leg_title[1],
      lwd = max(xl$lwd),
      col = ccol,
      val_rnd = leg_val_rnd[1],
      val_dec = leg_val_dec,
      val_big = leg_val_big
    )
    lg <- do.call(leg_comp, la1)
    la2 <- list(
      leg = lg,
      type = "typo",
      val = val_order,
      title = leg_title[2],
      col_na = col_na,
      no_data = no_data,
      no_data_txt = leg_no_data,
      pal = pal,
      box_cex = leg_box_cex * c(1, .5),
      box_border = pal
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

  # check merge and order
  dots <- create_dots(x = x, var = var1)

  # get modalities
  val_order <- get_modalities(
    x = dots[[var2]],
    val_order = val_order
  )
  # get color list and association
  pal <- get_the_pal(
    pal = pal, nbreaks = length(val_order),
    alpha = alpha, rev = !rev
  )
  # get color vector
  mycols <- get_col_typo(
    x = dots[[var2]], pal = pal,
    val_order = val_order
  )

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
    lwd = lwd, horiz = leg_horiz,
    self_adjust = TRUE
  )
  lg <- do.call(leg_comp, la1)
  pch <- ifelse(symbol == "circle", 21, 22)
  cex <- 2
  la2 <- list(
    leg = lg,
    type = "symb",
    val = val_order,
    title = leg_title[2],
    col_na = col_na,
    no_data = no_data,
    no_data_txt = leg_no_data,
    pal = pal,
    lwd = lwd,
    border = border,
    cex = cex, pch = pch,
    cex_na = cex,
    pch_na = pch,
    box_cex = c(.75, .75)
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
