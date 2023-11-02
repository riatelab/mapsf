#' @title Plot symbols using choropleth coloration
#' @description Plot symbols with colors based on a quantitative
#' data classification.
#' @eval my_params(c(
#' 'x',
#' 'var',
#' 'border',
#' 'lwd',
#' 'add' ,
#' 'col_na',
#' 'pal',
#' 'cexs',
#' 'pch',
#' 'pch_na',
#' 'cex_na',
#' 'val_order',
#' 'alpha',
#' 'rev',
#' 'breaks',
#' 'nbreaks',
#' 'leg_pos2',
#' 'leg_title',
#' 'leg_title_cex',
#' 'leg_val_cex',
#' 'leg_val_rnd',
#' 'leg_no_data',
#' 'leg_frame',
#' 'leg_horiz',
#' 'leg_frame_border',
#' 'leg_fg',
#' 'leg_bg',
#' 'leg_size',
#' 'leg_box_border',
#' 'leg_box_cex',
#' 'leg_adj'))
#' @details
#' Breaks defined by a numeric vector or a classification method are
#' left-closed: breaks defined by \code{c(2, 5, 10, 15, 20)}
#' will be mapped as [2 - 5[, [5 - 10[, [10 - 15[, [15 - 20].
#' The "jenks" method is an exception and has to be right-closed.
#' Jenks breaks computed as \code{c(2, 5, 10, 15, 20)}
#' will be mapped as [2 - 5], ]5 - 10], ]10 - 15], ]15 - 20].
#' @importFrom graphics box
#' @keywords internal
#' @export
#' @return x is (invisibly) returned.
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_symb_choro(mtq, c("STATUS", "MED"))
#'
#' mf_map(mtq)
#' mtq$STATUS[30] <- NA
#' mtq$MED[5] <- NA
#' mf_symb_choro(mtq, c("STATUS", "MED"),
#'   pal = "Reds 3", breaks = "quantile", nbreaks = 4,
#'   pch = 21:23, cex = c(3, 2, 1),
#'   pch_na = 25, cex_na = 1.5, col_na = "blue",
#'   val_order = c(
#'     "Prefecture",
#'     "Sub-prefecture",
#'     "Simple municipality"
#'   )
#' )
mf_symb_choro <- function(x, var,
                          pal = "Mint",
                          alpha = 1,
                          rev = FALSE,
                          breaks = "quantile",
                          nbreaks,
                          border = getOption("mapsf.fg"),
                          pch,
                          cex = 1,
                          lwd = .7,
                          pch_na = 4,
                          cex_na = 1,
                          col_na = "white",
                          val_order,
                          leg_pos = mf_get_leg_pos(x, 1),
                          leg_title = var,
                          leg_title_cex = c(.8, .8),
                          leg_val_cex = c(.6, .6),
                          leg_val_rnd = 2,
                          leg_no_data = c("No data", "No data"),
                          leg_frame = c(FALSE, FALSE),
                          leg_frame_border = getOption("mapsf.fg"),
                          leg_horiz = FALSE,
                          leg_adj = c(0, 0),
                          leg_fg = getOption("mapsf.fg"),
                          leg_bg = getOption("mapsf.bg"),
                          leg_size = 1,
                          leg_box_border = getOption("mapsf.fg"),
                          leg_box_cex = c(1, 1),
                          add = TRUE) {
  # default
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))

  xout <- x
  var2 <- var[2]
  var1 <- var[1]

  # Transform to point
  st_geometry(x) <- st_centroid(st_geometry(x), of_largest_polygon = TRUE)

  ################### COLORS ##########################
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
  ###################################################################
  ################## SYMBOLS  ######################################
  # get modalities
  val_order <- get_modalities(
    x = x[[var1]],
    val_order = val_order
  )

  if (missing(pch)) {
    pchs <- c(0:25, 32:127)
    pch <- pchs[seq_along(val_order)]
  }

  if (length(cex) != length(val_order)) {
    if (length(cex) != 1) {
      message(paste0(
        "the length of cex does not match the number of",
        "modalities. The first cex is used for all modalities"
      ))
    }
    cex <- rep(cex[1], length(val_order))
  }

  # get symbol list and association
  mysym <- get_sym_typo(
    x = x[[var1]], pch = pch,
    val_order = val_order
  )
  # TO BE DONE pch_NA ##################################
  mycex <- get_sym_typo(
    x = x[[var1]], pch = cex,
    val_order = val_order
  )
  # TO BE DONE symbol cex ##############################
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

  ##################################################################
  if (add == FALSE) {
    mf_init(x)
  }

  plot(st_geometry(x),
       col = mycolspt, bg = mycolsptbg, cex = mycex, pch = mysym,
       lwd = lwd, add = TRUE
  )

  leg_pos <- split_leg(leg_pos)
  if (length(leg_pos) == 1) {
    la1 <- list(
      type = "symb",
      val = val_order,
      title = leg_title[1],
      col_na = "grey",
      no_data = no_data[1],
      no_data_txt = leg_no_data[1],
      pal = rep("grey", length(val_order)),
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
             pos = leg_pos[[1]], bg = leg_bg, fg = leg_fg, size = leg_size,
             frame = leg_frame[1], title_cex = leg_title_cex[1],
             val_cex = leg_val_cex[1], mar = getOption("mapsf.mar"),
             adj = leg_adj, frame_border = leg_frame_border
    )
  } else {
    leg(
      type = "symb",
      pos = leg_pos[[1]],
      val = val_order,
      title = leg_title[1],
      title_cex = leg_title_cex[1],
      val_cex = leg_val_cex[1],
      col_na = "grey",
      no_data = no_data[1],
      no_data_txt = leg_no_data[1],
      frame = leg_frame[1],
      border = border,
      pal = rep("grey", length(val_order)),
      cex = cex,
      pch = pch,
      cex_na = cex_na,
      pch_na = pch_na,
      lwd = lwd,
      bg = leg_bg,
      fg = leg_fg,
      size = leg_size,
      frame_border = leg_frame_border
    )
    leg(
      type = "choro",
      pos = leg_pos[[2]],
      val = breaks,
      title = leg_title[2],
      title_cex = leg_title_cex[2],
      val_cex = leg_val_cex[2],
      val_rnd = leg_val_rnd,
      col_na = col_na,
      no_data = no_data[2],
      no_data_txt = leg_no_data[2],
      frame = leg_frame[2],
      frame_border = leg_frame_border,
      box_cex = leg_box_cex,
      box_border = leg_box_border,
      pal = pal,
      bg = leg_bg,
      fg = leg_fg,
      horiz = leg_horiz
    )
  }
  return(invisible(xout))
}
