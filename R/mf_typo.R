#' @title Plot a typology map
#' @description Plot a typology map.
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
#' 'leg_pos',
#' 'leg_title',
#' 'leg_title_cex',
#' 'leg_val_cex',
#' 'val_order',
#' 'leg_no_data',
#' 'leg_frame',
#' 'leg_frame_border',
#' 'leg_size',
#' 'leg_box_border',
#' 'leg_box_cex',
#' 'leg_fg',
#' 'leg_bg',
#' 'leg_adj'))
#' @param cex cex cex of the symbols if x is a POINT layer
#' @param pch pch type of pch if x is a POINT layer
#' @param pch_na pch for NA values if x is a POINT layer
#' @param cex_na cex for NA values if x is a POINT layer
#' @keywords internal
#' @export
#' @return No return value, a map is displayed.
#' @examples
#' mtq <- mf_get_mtq()
#' mf_typo(mtq, "STATUS")
#' mtq[6, "STATUS"] <- NA
#' mf_typo(
#'   x = mtq, var = "STATUS", pal = c("red", "blue", "yellow"), lwd = 1.1,
#'   val_order = c("Prefecture", "Sub-prefecture", "Simple municipality"),
#'   col_na = "green", border = "brown",
#'   leg_pos = "bottomleft",
#'   leg_title = "Status", leg_title_cex = 1.1,
#'   leg_val_cex = 1, leg_no_data = "No data",
#'   leg_frame = TRUE, add = FALSE
#' )
mf_typo <- function(x,
                    var,
                    pal = "Dynamic",
                    alpha = 1,
                    rev = FALSE,
                    val_order,
                    border = getOption("mapsf.fg"),
                    pch = 21,
                    cex = 1,
                    lwd = .7,
                    cex_na = 1,
                    pch_na = 4,
                    col_na = "white",
                    leg_pos = mf_get_leg_pos(x),
                    leg_title = var,
                    leg_title_cex = .8,
                    leg_val_cex = .6,
                    leg_no_data = "No data",
                    leg_frame = FALSE,
                    leg_frame_border = getOption("mapsf.fg"),
                    leg_adj = c(0, 0),
                    leg_size = 1,
                    leg_box_border = getOption("mapsf.fg"),
                    leg_box_cex = c(1, 1),
                    leg_fg = getOption("mapsf.fg"),
                    leg_bg = getOption("mapsf.bg"),
                    add = FALSE) {
  # default
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))

  # get modalities
  val_order <- get_modalities(
    x = x[[var]],
    val_order = val_order
  )

  # get color list and association
  pal <- get_the_pal(pal = pal, nbreaks = length(val_order),
                     alpha = alpha, rev = !rev)
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
    mf_init(x)
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
      frame = leg_frame, border = border, pal = pal, lwd = lwd,
      cex = rep(cex, n), pch = rep(pch, n), cex_na = cex_na,
      pch_na = pch_na, bg = leg_bg, fg = leg_fg, adj = leg_adj,
      size = leg_size, frame_border = leg_frame_border
    )
  }

  return(invisible(x))
}
