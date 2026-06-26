symb <- function(x, var,
                    pal,
                    alpha = NULL,
                    rev = FALSE,
                    border,
                    pch,
                    cex = 2,
                    lwd = .7,
                    col_na = "grey",
                    pch_na = 4,
                    cex_na = 1,
                    val_order,
                    extent = x,
                    bg,
                    expandBB = rep(0, 4),
                    leg_pos = mf_get_leg_pos(x),
                    leg_title = var,
                    leg_title_cex = .8,
                    leg_val_cex = .6,
                    leg_no_data = "No data",
                    leg_frame = FALSE,
                    leg_frame_border,
                    leg_adj = c(0, 0),
                    leg_fg,
                    leg_bg,
                    leg_size = 1,
                    add = TRUE) {
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
  border <- go(border, "background")
  leg_fg <- go(leg_fg, "highlight")
  leg_bg <- go(leg_bg, "foreground", getOption("mapsf.background"))
  leg_frame_border <- go(
    leg_frame_border, "foreground",
    getOption("mapsf.highlight")
  )
  pal <- go(pal, "pal_quali", "Dynamic")


  xout <- x
  # Transform to point
  st_geometry(x) <- st_centroid(st_geometry(x), of_largest_polygon = TRUE)

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

  if (missing(pch)) {
    pchs <- c(21:25, 0:20, 32:127)
    pch <- pchs[seq_along(val_order)]
  }
  if (length(pch) != length(val_order)) {
    message(paste0(
      "The length of pch does not match the number of ",
      "modalities. The first pch is used for all modalities."
    ))
    pch <- rep(pch[1], length(val_order))
  }

  if (length(cex) != length(val_order)) {
    if (length(cex) != 1) {
      message(paste0(
        "The length of cex does not match the number of ",
        "modalities. The first cex is used for all modalities."
      ))
    }
    cex <- rep(cex[1], length(val_order))
  }

  # get symbol vector
  mysym <- get_sym_typo(
    x = x[[var]],
    pch = pch,
    val_order = val_order
  )
  # get symbol vector
  mycex <- get_sym_typo(
    x = x[[var]],
    pch = cex,
    val_order = val_order
  )

  no_data <- FALSE
  if (max(is.na(mycols)) == 1) {
    no_data <- TRUE
  }
  mycols[is.na(mycols)] <- col_na
  mysym[is.na(mysym)] <- pch_na
  mycex[is.na(mycex)] <- cex_na


  border <- border[[1]]
  lwd <- lwd[[1]]

  mycolspt <- mycols
  mycolspt[mysym %in% 21:25] <- border
  mycolsptbg <- mycols
  border <- rep(border, length(cex))

  if (add == FALSE) {
    mf_ini(x, expandBB = expandBB, extent = extent, bgc = bgc)
  }

  plot(st_geometry(x),
    col = mycolspt, bg = mycolsptbg, cex = mycex, pch = mysym,
    lwd = lwd, add = TRUE
  )

  leg(
    type = "symb",
    pos = leg_pos, val = val_order, title = leg_title,
    title_cex = leg_title_cex,
    val_cex = leg_val_cex, col_na = col_na, no_data = no_data,
    no_data_txt = leg_no_data,
    frame = leg_frame, border = border, pal = pal, lwd = lwd,
    cex = cex, pch = pch, cex_na = cex_na,
    pch_na = pch_na, bg = leg_bg, fg = leg_fg, adj = leg_adj,
    size = leg_size, frame_border = leg_frame_border
  )
  return(invisible(xout))
}
