#' @title Plot a choropleth map
#' @description Plot choropleth map.
#' @eval my_params(c(
#' 'x',
#' 'var',
#' 'border',
#' 'lwd',
#' 'add' ,
#' 'col_na',
#' 'pal',
#' 'alpha',
#' 'rev',
#' 'breaks',
#' 'nbreaks',
#' 'leg_pos',
#' 'leg_title',
#' 'leg_title_cex',
#' 'leg_val_cex',
#' 'leg_val_rnd',
#' 'leg_no_data',
#' 'leg_frame',
#' 'leg_adj',
#' 'leg_horiz',
#' 'leg_size',
#' 'leg_box_border',
#' 'leg_box_cex',
#' 'leg_fg',
#' 'leg_bg',
#' 'leg_frame_border'))
#' @param cex cex cex of the symbols if x is a POINT layer
#' @param pch pch type of pch if x is a POINT layer
#' @param pch_na pch for NA values if x is a POINT layer
#' @param cex_na cex for NA values if x is a POINT layer
#' @details
#' Breaks defined by a numeric vector or a classification method are
#' left-closed: breaks defined by \code{c(2, 5, 10, 15, 20)}
#' will be mapped as [2 - 5[, [5 - 10[, [10 - 15[, [15 - 20].
#' The "jenks" method is an exception and has to be right-closed.
#' Jenks breaks computed as \code{c(2, 5, 10, 15, 20)}
#' will be mapped as [2 - 5], ]5 - 10], ]10 - 15], ]15 - 20].
#' @keywords internal
#' @export
#' @return x is (invisibly) returned.
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq, var = "MED", type = "choro")
#'
#' mtq[6, "MED"] <- NA
#' mf_map(
#'   x = mtq, var = "MED", type = "choro",
#'   col_na = "grey", pal = "Cividis",
#'   breaks = "quantile", nbreaks = 4, border = "white",
#'   lwd = .5, leg_pos = "topleft",
#'   leg_title = "Median Income", leg_title_cex = 1.1,
#'   leg_val_cex = 1, leg_val_rnd = -2, leg_no_data = "No data",
#'   leg_frame = TRUE
#' )
mf_choro <- function(x, var,
                     pal = "Mint",
                     alpha = 1,
                     rev = FALSE,
                     breaks = "quantile",
                     nbreaks,
                     border = getOption("mapsf.fg"),
                     pch = 21,
                     cex = 1,
                     lwd = .7,
                     col_na = "white",
                     cex_na = 1,
                     pch_na = 4,
                     leg_pos = mf_get_leg_pos(x),
                     leg_title = var,
                     leg_title_cex = .8,
                     leg_val_cex = .6,
                     leg_val_rnd = 2,
                     leg_no_data = "No data",
                     leg_frame = FALSE,
                     leg_horiz = FALSE,
                     leg_adj = c(0, 0),
                     leg_size = 1,
                     leg_box_border = getOption("mapsf.fg"),
                     leg_box_cex = c(1, 1),
                     leg_fg = getOption("mapsf.fg"),
                     leg_bg = getOption("mapsf.bg"),
                     leg_frame_border = getOption("mapsf.fg"),
                     add = FALSE) {
  # default
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))

  # jenks
  jen <- FALSE
  if (any(breaks %in% "jenks")) {
    jen <- TRUE
  }

  # get the breaks
  breaks <- mf_get_breaks(x = x[[var]], nbreaks = nbreaks, breaks = breaks)
  nbreaks <- length(breaks) - 1
  # get the cols
  pal <- get_the_pal(pal = pal, nbreaks = nbreaks, alpha = alpha, rev = !rev)
  # get the color vector
  mycols <- get_col_vec(x = x[[var]], breaks = breaks, pal = pal, jen = jen)

  no_data <- FALSE
  if (max(is.na(mycols)) == 1) {
    no_data <- TRUE
  }
  mycols[is.na(mycols)] <- col_na


  if (add == FALSE) {
    mf_init(x)
  }

  xtype <- get_geom_type(x)
  if (xtype == "LINE") {
    plot(st_geometry(x), col = mycols, lwd = lwd, add = TRUE)
  }
  if (xtype == "POLYGON") {
    plot(
      st_geometry(x),
      col = mycols, border = border, lwd = lwd,
      add = TRUE
    )
  }
  if (xtype == "POINT") {
    if (pch %in% 21:25) {
      mycolspt <- border
    } else {
      mycolspt <- mycols
    }
    mycolsptbg <- mycols
    plot(
      st_geometry(x),
      col = mycolspt, bg = mycolsptbg, cex = cex, pch = pch,
      lwd = lwd, add = TRUE
    )
  }

  leg(
    type = "choro",
    pos = leg_pos,
    val = breaks,
    title = leg_title,
    title_cex = leg_title_cex,
    val_cex = leg_val_cex,
    val_rnd = leg_val_rnd,
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

  return(invisible(x))
}
