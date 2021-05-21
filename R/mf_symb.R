#' @title Plot symbols
#' @description Plot symbols based on qualitative data.
#' @eval my_params(c(
#' 'x',
#' 'var',
#' 'border',
#' 'lwd',
#' 'add' ,
#' 'col_na',
#' 'cex_na',
#' 'pch_na',
#' 'pal',
#' 'alpha',
#' 'leg_pos',
#' 'leg_title',
#' 'leg_title_cex',
#' 'leg_val_cex',
#' 'leg_val_rnd',
#' 'leg_no_data',
#' 'leg_frame'))
#' @param cex cex for symbols
#' @param pch pch for symbols
#' @param val_order val order
#' @importFrom methods is
#' @importFrom graphics box
#' @keywords internal
#' @export
#' @return No return value, a map is displayed.
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_symb(mtq, "STATUS")
#'
#' mtq[6, "STATUS"] <- NA
#' mf_map(mtq)
#' mf_symb(
#'   x = mtq, var = "STATUS", pch = c(21:23), pal = c("red1", "tan1", "khaki1"),
#'   border = "grey20", cex = c(1.5, 1, .9), lwd = .5,
#'   val_order = c("Prefecture", "Sub-prefecture", "Simple municipality"),
#'   pch_na = 24, leg_frame = TRUE
#' )
mf_symb <- function(x, var,
                    pal = "Dynamic",
                    alpha = 1,
                    border,
                    pch,
                    cex = 1,
                    lwd = .7,
                    col_na = "grey",
                    pch_na = 4,
                    cex_na = 1,
                    val_order,
                    leg_pos = mf_get_leg_pos(x),
                    leg_title = var,
                    leg_title_cex = .8,
                    leg_val_cex = .6,
                    leg_val_rnd = 2,
                    leg_no_data = "No data",
                    leg_frame = FALSE,
                    add = TRUE) {

  # default
  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  on.exit(par(op))
  bg <- .gmapsf$args$bg
  fg <- .gmapsf$args$fg
  if (missing(border)) border <- fg

  # Transform to point
  st_geometry(x) <- st_centroid(st_geometry(x), of_largest_polygon = TRUE)

  ################### COLORS ##########################
  # get modalities
  val_order <- get_modalities(
    x = x[[var]],
    val_order = val_order
  )
  # get color list and association
  pal <- get_the_pal(pal = pal, nbreaks = length(val_order), alpha = alpha)
  # get color vector
  mycols <- get_col_typo(
    x = x[[var]], pal = pal,
    val_order = val_order
  )

  if (missing(pch)) {
    pchs <- c(0:25, 32:127)
    pch <- pchs[1:length(val_order)]
  }
  if (length(pch) != length(val_order)) {
    message(paste0(
      "the length of pch does not match the number of ",
      "modalities. The first pch is used for all modalities"
    ))
    pch <- rep(pch[1], length(val_order))
  }

  if (length(cex) != length(val_order)) {
    if (length(cex) != 1) {
      message(paste0(
        "the length of cex does not match the number of ",
        "modalities. The first cex is used for all modalities"
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


  mycolspt <- mycols
  mycolspt[mysym %in% 21:25] <- border
  mycolsptbg <- mycols

  ##################################################################
  if (add == FALSE) {
    mf_init(x)
    add <- TRUE
  }

  plot(st_geometry(x),
    col = mycolspt, bg = mycolsptbg, cex = mycex, pch = mysym,
    lwd = lwd, add = add
  )

  mf_legend_s(
    pos = leg_pos, val = val_order, title = leg_title, title_cex = leg_title_cex,
    val_cex = leg_val_cex, col_na = col_na, no_data = no_data,
    no_data_txt = leg_no_data,
    frame = leg_frame, border = border, pal = pal, lwd = lwd,
    pt_cex = cex, pt_pch = pch, pt_cex_na = cex_na,
    pt_pch_na = pch_na, bg = bg, fg = fg
  )
  return(invisible(NULL))
}
