typo <- function(x,
                 var,
                 pal,
                 alpha = NULL,
                 rev = FALSE,
                 val_order,
                 border,
                 pch = 21,
                 cex = 2,
                 lwd = .7,
                 col_na = "white",
                 extent = x,
                 bg,
                 expandBB = rep(0, 4),
                 leg_pos = "top",
                 leg_title = var,
                 leg_title_cex = .8,
                 leg_val_cex = .6,
                 leg_no_data = "No data",
                 leg_frame = FALSE,
                 leg_frame_border,
                 leg_adj = c(0, 0),
                 leg_size = 1,
                 leg_box_border,
                 leg_box_cex = c(1, 1),
                 leg_fg,
                 leg_bg,
                 add) {
  # default
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))

  bgc <- go(bg, "background")
  pal <- go(pal, "pal_quali")
  leg_box_border <- go(leg_box_border, "highlight")
  leg_fg <- go(leg_fg, "highlight")
  leg_bg <- go(leg_bg, "foreground")
  leg_frame_border <- go(leg_frame_border, "foreground")

  lwd <- lwd[[1]]
  cex <- cex[[1]]
  pch <- pch[[1]]

  # get modalities
  val_order <- get_modalities(
    x = x[[var]],
    val_order = val_order
  )

  # get color list and association
  pal <- get_the_pal(
    pal = pal, nbreaks = length(val_order),
    alpha = alpha, rev = !rev
  )
  # get color vector
  mycols <- get_col_typo(
    x = x[[var]], pal = pal,
    val_order = val_order
  )

  no_data <- FALSE
  if (max(is.na(mycols)) == 1) {
    no_data <- TRUE
  }
  mycols[is.na(mycols)] <- col_na

  if (add == FALSE) {
    mf_ini(x, expandBB = expandBB, extent = extent, bgc = bgc)
  }

  xtype <- get_geom_type(x)
  if (xtype == "LINE") {
    plot(st_geometry(x), col = mycols, lwd = lwd, add = TRUE)
    leg(
      type = "typo",
      pos = leg_pos,
      val = val_order,
      title = leg_title,
      title_cex = leg_title_cex,
      val_cex = leg_val_cex,
      col_na = col_na,
      no_data = no_data,
      no_data_txt = leg_no_data,
      box_border = NA,
      frame = leg_frame,
      frame_border = leg_frame_border,
      pal = pal,
      bg = leg_bg,
      fg = leg_fg,
      box_cex = c(1.2, .1),
      adj = leg_adj,
      size = leg_size,
    )
  }
  if (xtype == "POLYGON") {
    border <- go(border, opt = "highlight")[[1]]
    plot(st_geometry(x),
      col = mycols, border = border,
      lwd = lwd, add = TRUE
    )
    leg(
      type = "typo",
      pos = leg_pos,
      val = val_order,
      title = leg_title,
      title_cex = leg_title_cex,
      val_cex = leg_val_cex,
      col_na = col_na,
      no_data = no_data,
      no_data_txt = leg_no_data,
      size = leg_size,
      box_border = leg_box_border,
      box_cex = leg_box_cex,
      frame_border = leg_frame_border,
      frame = leg_frame,
      pal = pal,
      bg = leg_bg,
      fg = leg_fg,
      adj = leg_adj
    )
  }
  if (xtype == "POINT") {
    border <- go(border, "background")[[1]]
    if (pch %in% 21:25) {
      mycolspt <- border
    } else {
      mycolspt <- mycols
    }
    mycolsptbg <- mycols
    plot(st_geometry(x),
      col = mycolspt, bg = mycolsptbg, cex = cex, pch = pch,
      lwd = lwd, add = TRUE
    )
    n <- length(val_order)
    leg(
      type = "symb",
      pos = leg_pos, val = val_order, title = leg_title,
      title_cex = leg_title_cex,
      val_cex = leg_val_cex, col_na = col_na, no_data = no_data,
      no_data_txt = leg_no_data,
      frame = leg_frame, border = rep(border, n), pal = pal, lwd = lwd,
      cex = rep(cex, n), pch = rep(pch, n), cex_na = cex,
      pch_na = pch, bg = leg_bg, fg = leg_fg, adj = leg_adj,
      size = leg_size, frame_border = leg_frame_border
    )
  }

  return(invisible(x))
}
