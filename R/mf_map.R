#' @title Plot a map
#' @description This is the main function of the package.
#' mf_map can be used to plot all types of maps.
#' The three main arguments are: `x` (sf object), `var` (variable to map), and
#' `type` (map type).
#'
#' Relevant arguments and default values are detailed in specific functions.
#'
#' Maps types:
#' * **base**, base maps (\link{mf_base});
#' * **prop**, proportional symbols maps (\link{mf_prop});
#' * **choro**, choropleth maps (\link{mf_choro});
#' * **typo**, typology maps (\link{mf_typo});
#' * **symb**, symbols maps (\link{mf_symb});
#' * **grad**, graduated symbols maps (\link{mf_grad});
#' * **prop_choro**, proportional symbols maps with symbols colors based
#' on a quantitative data classification (\link{mf_prop_choro});
#' * **prop_typo**, proportional symbols maps with symbols colors based
#' on qualitative data (\link{mf_prop_typo});
#' * **symb_choro**, symbols maps with symbols colors based on
#' a quantitative data classification (\link{mf_symb_choro}).
#' @md
#'
#'
#' @eval my_params(c(
#' "xfull",
#' "var",
#' "pal",
#' "breaks",
#' "nbreaks",
#' "border",
#' "lwd",
#' "bg",
#' "col",
#' "lwd_max",
#' "col_na",
#' "cex_na",
#' "pch_na",
#' 'leg_pos',
#' 'leg_title',
#' 'leg_title_cex',
#' 'leg_val_cex',
#' 'leg_val_rnd',
#' 'leg_no_data',
#' 'leg_frame',
#' 'add',
#' 'inches',
#' 'val_max',
#' 'symbol',
#' 'val_order'))
#' @param type one of "base", "prop", "choro", "typo", "symb", "grad",
#' "prop_choro", "prop_typo", "symb_choro"
#' @param cex cex for symbols
#' @param pch pch for symbols
#' @param ... further parameters from \link{plot} for sfc objects
#' @export
#' @return x is (invisibly) returned.
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_map(mtq, var = "POP", type = "prop")
#' mf_map(mtq, var = "MED", type = "choro")
#' mf_map(mtq, var = "STATUS", type = "typo")
#' mf_map(mtq)
#' mf_map(mtq, var = "STATUS", type = "symb")
#' mf_map(mtq)
#' mf_map(mtq, var = "POP", type = "grad")
#' mf_map(mtq)
#' mf_map(mtq, var = c("POP", "MED"), type = "prop_choro")
#' mf_map(mtq)
#' mf_map(mtq, var = c("POP", "STATUS"), type = "prop_typo")
#' mf_map(mtq)
#' mf_map(mtq, var = c("STATUS", "MED"), type = "symb_choro")
mf_map <- function(x, var, type = "base",
                   breaks, nbreaks, pal,
                   inches, val_max, symbol, col,
                   lwd_max, val_order, pch, cex,
                   border, lwd, bg,
                   col_na, cex_na, pch_na,
                   leg_pos, leg_title, leg_title_cex,
                   leg_val_cex, leg_val_rnd, leg_no_data,
                   leg_frame, add,
                   ...) {
  argx <- as.list(match.call()[-1])
  argx <- argx[names(argx) != "type"]

  switch(type,
    prop = do.call(what = mf_prop, argx, envir = parent.frame()),
    choro = do.call(what = mf_choro, argx, envir = parent.frame()),
    typo = do.call(what = mf_typo, argx, envir = parent.frame()),
    symb = do.call(what = mf_symb, argx, envir = parent.frame()),
    base = do.call(what = mf_base, argx, envir = parent.frame()),
    grad = do.call(what = mf_grad, argx, envir = parent.frame()),
    prop_choro = do.call(what = mf_prop_choro, argx, envir = parent.frame()),
    prop_typo = do.call(what = mf_prop_typo, argx, envir = parent.frame()),
    symb_choro = do.call(what = mf_symb_choro, argx, envir = parent.frame())
  )

  return(invisible(x))
}
