#' @title Plot a map
#' @description This is the main function of the package.
#' mf_map can be used to plot all types of maps.
#' The three main arguments are: `x` (sf object), `var` (variable to map), and
#' `type` (map type).
#'
#' Relevant arguments and default values for each map types are detailed in
#' specific functions:
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
#' @eval my_params(c(
#' "xfull",
#' "var",
#' "pal",
#' "alpha",
#' "breaks",
#' "nbreaks",
#' "border",
#' "lwd",
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
#' @param cex cex (point size) for symbols
#' @param pch pch (point type) for symbols
#' @param expandBB fractional values to expand the bounding box with, in each
#' direction (bottom, left, top, right)
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
                   breaks, nbreaks, pal, alpha = 1,
                   inches, val_max, symbol, col,
                   lwd_max, val_order, pch, cex,
                   border, lwd,
                   col_na, cex_na, pch_na,
                   leg_pos, leg_title, leg_title_cex,
                   leg_val_cex, leg_val_rnd, leg_no_data,
                   leg_frame, expandBB, add,
                   ...) {
  # check args
  if (!type %in% c(
    "base", "prop", "choro", "typo", "symb", "grad",
    "prop_choro", "prop_typo", "symb_choro"
  )) {
    stop(
      paste0(
        '\'type\' should be one of "base", "prop", "choro", "typo", ',
        '"symb", "grad", "prop_choro", "prop_typo" or "symb_choro".'
      ),
      call. = FALSE
    )
  }

  cl <- inherits(x = x, what = c("sf", "sfc", "sfg"), which = TRUE) != 0
  if (cl[1] == FALSE & cl[2] == TRUE & type != "base") {
    stop(paste0("'x' should be an sf object."), call. = FALSE)
  }
  if (cl[1] == FALSE & cl[2] == FALSE & cl[3] == FALSE) {
    stop(paste0("'x' should be an sf, sfc or sfg object."), call. = FALSE)
  }

  if (!missing(var)) {
    if (type == "base") {
      message("Please use the 'type' argument to map variables.")
    } else {
      lv <- length(var)
      lin <- var %in% names(x)
      if (lv != length(lin[lin == TRUE])) {
        stop(paste0("It is likely that 'var' is not a valid variable name."),
             call. = FALSE
        )
      }
    }
  }

  # add mgmgt, set default add, do not add if no device is launch
  if(missing(add)){
    add <- switch(type,
                  prop = TRUE,
                  choro = FALSE,
                  typo = FALSE,
                  symb = TRUE,
                  base = FALSE,
                  grad = TRUE,
                  prop_choro = TRUE,
                  prop_typo = TRUE,
                  symb_choro = TRUE)
  }


  if(is.null(grDevices::dev.list())){
    add <- FALSE
  }


  argx <- as.list(match.call()[-1])
  argx <- argx[!names(argx) %in% c("type", "expandBB")]


  if (!missing(expandBB) && !add){
    mf_init(x, expandBB = expandBB)
    argx$add <- TRUE
  } else {
    argx$add <- add
  }



  x <- do.call(what = get(paste0("mf_",type)), argx, envir = parent.frame())

 return(invisible(x))
}
