symb_choro <- function(x, var,
                          pal,
                          alpha = NULL,
                          rev = FALSE,
                          breaks = "quantile",
                          nbreaks,
                          border,
                          pch,
                          cex = 2,
                          lwd = .7,
                          pch_na = 4,
                          cex_na = 1,
                          col_na = "white",
                          val_order,
                          extent = x,
                          bg,
                          expandBB = rep(0, 4),
                          leg_pos = "top",
                          leg_title = var,
                          leg_title_cex = .8,
                          leg_val_cex = .6,
                          leg_val_rnd = 2,
                          leg_val_dec = ".",
                          leg_val_big = "",
                          leg_no_data = c("No data", "No data"),
                          leg_frame = FALSE,
                          leg_frame_border,
                          leg_horiz = FALSE,
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
  border <- go(border, "background")
  leg_box_border <- go(leg_box_border, "highlight")
  leg_fg <- go(leg_fg, "highlight")
  leg_bg <- go(leg_bg, "foreground")
  leg_frame_border <- go(leg_frame_border, "foreground")
  pal <- go(pal, "pal_seq")


  xout <- x
  var2 <- var[2]
  var1 <- var[1]

  # Transform to point
  st_geometry(x) <- st_centroid(st_geometry(x), of_largest_polygon = TRUE)

  # jenks
  jen <- FALSE
  if (any(breaks %in% "jenks")) {
    jen <- TRUE
  }
  # get the breaks
  breaks <- mf_get_breaks(x = x[[var2]], nbreaks = nbreaks, breaks = breaks)
  nbreaks <- length(breaks) - 1
  # get the cols
  pal <- get_the_pal(pal = pal, nbreaks = nbreaks, alpha = alpha, rev = !rev)
  # get the color vector
  mycols <- get_col_vec(x = x[[var2]], breaks = breaks, pal = pal, jen = jen)

  no_data <- c(FALSE, FALSE)
  if (max(is.na(mycols)) == 1) {
    no_data[2] <- TRUE
  }
  mycols[is.na(mycols)] <- col_na

  # get modalities
  val_order <- get_modalities(
    x = x[[var1]],
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

  # get symbol list and association
  mysym <- get_sym_typo(
    x = x[[var1]], pch = pch,
    val_order = val_order
  )

  mycex <- get_sym_typo(
    x = x[[var1]], pch = cex,
    val_order = val_order
  )

  if (max(is.na(mysym)) == 1) {
    no_data[1] <- TRUE
  }
  mysym[is.na(mysym)] <- pch_na
  mycex[is.na(mycex)] <- cex_na

  border <- border[[1]]
  lwd <- lwd[[1]]

  mycolspt <- mycols
  mycolspt[mysym %in% 21:25] <- border
  mycolsptbg <- mycols

  if (add == FALSE) {
    mf_ini(x, expandBB = expandBB, extent = extent, bgc = bgc)
  }

  plot(st_geometry(x),
       col = mycolspt, bg = mycolsptbg, cex = mycex, pch = mysym,
       lwd = lwd, add = TRUE
  )

  leg_pos <- defunct_leg(leg_pos)
  if (is.null(getOption("mapsf.legacy"))) {
    ccol <- getOption("mapsf.foreground")
  } else {
    ccol <- "grey80"
  }
  if (all(leg_frame, !leg_horiz, is.null(getOption("mapsf.legacy")))) {
    ccol <- getOption("mapsf.highlight")
  }
  border <- rep(border, length(cex))

  leg_title <- test2args(leg_title)
  leg_no_data <- test2args(leg_no_data)

  la1 <- list(
    type = "symb",
    val = val_order,
    title = leg_title[1],
    col_na = ccol,
    no_data = no_data[1],
    no_data_txt = leg_no_data[1],
    pal = rep(ccol, length(val_order)),
    border = border,
    cex = cex,
    pch = pch,
    lwd = lwd,
    cex_na = cex_na,
    pch_na = pch_na
  )
  lg <- do.call(leg_comp, la1)
  la2 <- list(
    leg = lg,
    type = "choro",
    val = breaks,
    title = leg_title[2],
    val_rnd = leg_val_rnd,
    val_dec = leg_val_dec,
    val_big = leg_val_big,
    col_na = col_na,
    no_data = no_data[2],
    no_data_txt = leg_no_data[2],
    pal = pal,
    horiz = leg_horiz,
    box_border = leg_box_border,
    box_cex = leg_box_cex
  )
  lg <- do.call(leg_comp, la2)
  leg_draw(lg,
           pos = leg_pos, bg = leg_bg, fg = leg_fg, size = leg_size,
           frame = leg_frame, title_cex = leg_title_cex,
           val_cex = leg_val_cex, mar = getOption("mapsf.mar"),
           adj = leg_adj, frame_border = leg_frame_border
  )
  return(invisible(xout))
}
