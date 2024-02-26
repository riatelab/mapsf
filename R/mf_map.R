#' @title Plot a map
#' @description
#' `mf_map()` is the main function of the package, it displays map layers on a
#' georeferenced plot.
#'
#' `mf_map()` has three main arguments:
#' * `x`, an sf object;
#' * `var`, the name(s) of a variable(s) to map;
#' * `type`, the map layer type.
#'
#' Many parameters are available to fine tune symbologies and legends.
#'
#' Relevant arguments and default values are different for each map type and are
#' described in the "Details" section.
#'
#'
#' @usage
#' mf_map(x, var, type = "base",
#'        breaks, nbreaks, pal, alpha, rev, inches, val_max, symbol, col,
#'        lwd_max, val_order, pch, cex, border, lwd, col_na, cex_na, pch_na,
#'        expandBB, add,
#'        leg_pos, leg_title, leg_title_cex, leg_val_cex, leg_val_rnd,
#'        leg_no_data, leg_frame, leg_frame_border, leg_horiz, leg_adj, leg_bg,
#'        leg_fg, leg_size, leg_border, leg_box_border, leg_box_cex, ...)
#'
#'
#'
#' @details
#' ## Relevant arguments and default values for each map types:
#' **base**: displays sf objects geometries.
#' \preformatted{
#' mf_map(x, col = "grey80", pch = 20, cex = 1, border = "grey20", lwd = 0.7,
#'        expandBB, add = FALSE, ...)
#'        }
#'
#' **prop**: displays symbols with areas proportional to a quantitative
#' variable (stocks). `inches` is used to set symbols sizes.
#' \preformatted{
#' mf_map(x, var, type = "prop", inches = 0.3, val_max, symbol = "circle",
#'        col = "tomato4", lwd_max = 20, border = getOption("mapsf.fg"),
#'        lwd = 0.7, expandBB, add = TRUE,
#'        leg_pos = mf_get_leg_pos(x), leg_title = var,
#'        leg_title_cex = 0.8, leg_val_cex = 0.6, leg_val_rnd = 0,
#'        leg_frame = FALSE, leg_frame_border = getOption("mapsf.fg"),
#'        leg_horiz = FALSE, leg_adj = c(0, 0),
#'        leg_bg = getOption("mapsf.bg"), leg_fg = getOption("mapsf.fg"),
#'        leg_size = 1)
#'        }
#'
#' **choro**: areas are shaded according to the variation of a quantitative
#' variable. Choropleth maps are used to represent ratios or indices.
#' `nbreaks`, and `breaks` allow to set the variable classification.
#' Colors palettes, defined with `pal`, can be created with `mf_get_pal()` or
#' can use palette names from `hcl.pals()`.
#' \preformatted{
#' mf_map(x, var, type = "choro", breaks = "quantile", nbreaks, pal = "Mint",
#'        alpha = 1, rev = FALSE, pch = 21, cex = 1,
#'        border = getOption("mapsf.fg"), lwd = 0.7, col_na = "white",
#'        cex_na = 1, pch_na = 4, expandBB, add = FALSE,
#'        leg_pos = mf_get_leg_pos(x), leg_title = var, leg_title_cex = 0.8,
#'        leg_val_cex = 0.6, leg_val_rnd = 2, leg_no_data = "No data",
#'        leg_frame = FALSE, leg_frame_border = getOption("mapsf.fg"),
#'        leg_horiz = FALSE, leg_adj = c(0, 0), leg_bg = getOption("mapsf.bg"),
#'        leg_fg = getOption("mapsf.fg"), leg_size = 1,
#'        leg_box_border = getOption("mapsf.fg"), leg_box_cex = c(1, 1))
#'        }
#'
#' **typo**: displays a typology map of a qualitative variable.
#' `val_order` is used to set modalities order in the legend.
#' \preformatted{
#' mf_map(x, var, type = "typo", pal = "Dynamic", alpha = 1, rev = FALSE,
#'        val_order,border = getOption("mapsf.fg"), pch = 21, cex = 1,
#'        lwd = 0.7, cex_na = 1, pch_na = 4, col_na = "white",
#'        leg_pos = mf_get_leg_pos(x), leg_title = var, leg_title_cex = 0.8,
#'        leg_val_cex = 0.6, leg_no_data = "No data", leg_frame = FALSE,
#'        leg_frame_border = getOption("mapsf.fg"), leg_adj = c(0, 0),
#'        leg_size = 1, leg_box_border = getOption("mapsf.fg"),
#'        leg_box_cex = c(1, 1), leg_fg = getOption("mapsf.fg"),
#'        leg_bg = getOption("mapsf.bg"), add = FALSE)
#'        }
#'
#' **symb**: displays the different modalities of a qualitative variable as
#' symbols.
#' \preformatted{
#' mf_map(x, var, type = "symb", pal = "Dynamic", alpha = 1, rev = FALSE,
#'        border = getOption("mapsf.fg"), pch, cex = 1, lwd = 0.7,
#'        col_na = "grey", pch_na = 4, cex_na = 1, val_order,
#'        leg_pos = mf_get_leg_pos(x), leg_title = var, leg_title_cex = 0.8,
#'        leg_val_cex = 0.6, leg_val_rnd = 2, leg_no_data = "No data",
#'        leg_frame = FALSE, leg_frame_border = getOption("mapsf.fg"),
#'        leg_adj = c(0, 0), leg_fg = getOption("mapsf.fg"),
#'        leg_bg = getOption("mapsf.bg"), leg_size = 1, add = TRUE)
#'        }
#'
#' **grad**: displays graduated symbols. Sizes classes are set with
#' `breaks` and `nbreaks`. Symbol sizes are set with `cex`.
#' \preformatted{
#' mf_map(x, var, type = "grad", breaks = "quantile", nbreaks = 3, col = "tomato4",
#'        border = getOption("mapsf.fg"), pch = 21, cex, lwd,
#'        leg_pos = mf_get_leg_pos(x), leg_title = var, leg_title_cex = 0.8,
#'        leg_val_cex = 0.6, leg_val_rnd = 2, leg_frame = FALSE,
#'        leg_adj = c(0, 0), leg_size = 1, leg_border = border,
#'        leg_box_cex = c(1, 1), leg_fg = getOption("mapsf.fg"),
#'        leg_bg = getOption("mapsf.bg"), leg_frame_border = getOption("mapsf.fg"),
#'        add = TRUE)
#'        }
#'
#' **prop_choro**: displays symbols with sizes proportional to values of a
#' first variable and colored to reflect the classification of a second
#' quantitative variable.
#' \preformatted{
#' mf_map(x, var, type = "prop_choro", inches = 0.3, val_max, symbol = "circle",
#'        pal = "Mint", alpha = 1, rev = FALSE, breaks = "quantile", nbreaks,
#'        border = getOption("mapsf.fg"), lwd = 0.7, col_na = "white",
#'        leg_pos = mf_get_leg_pos(x, 1), leg_title = var,
#'        leg_title_cex = c(0.8, 0.8), leg_val_cex = c(0.6, 0.6),
#'        leg_val_rnd = c(0, 2), leg_no_data = "No data",
#'        leg_frame = c(FALSE, FALSE), leg_frame_border = getOption("mapsf.fg"),
#'        leg_horiz = c(FALSE, FALSE), leg_adj = c(0, 0),
#'        leg_fg = getOption("mapsf.fg"), leg_bg = getOption("mapsf.bg"),
#'        leg_size = 1, leg_box_border = getOption("mapsf.fg"),
#'        leg_box_cex = c(1, 1), add = TRUE)
#'        }
#'
#' **prop_typo**: displays symbols with sizes proportional to values of a
#' first variable and colored to reflect the modalities of a second qualitative
#' variable.
#' \preformatted{
#' mf_map(x, var, type = "prop_typo", inches = 0.3, val_max, symbol = "circle",
#'        pal = "Dynamic", alpha = 1, rev = FALSE, val_order,
#'        border = getOption("mapsf.fg"), lwd = 0.7, lwd_max = 15,
#'        col_na = "white",
#'        leg_pos = mf_get_leg_pos(x, 1), leg_title = var,
#'        leg_title_cex = c(0.8, 0.8), leg_val_cex = c(0.6, 0.6),
#'        leg_val_rnd = c(0), leg_no_data = "No data", leg_frame = c(FALSE, FALSE),
#'        leg_frame_border = getOption("mapsf.fg"), leg_horiz = FALSE,
#'        leg_adj = c(0, 0), leg_fg = getOption("mapsf.fg"),
#'        leg_bg = getOption("mapsf.bg"), leg_size = 1,
#'        leg_box_border = getOption("mapsf.fg"), leg_box_cex = c(1, 1),
#'        add = TRUE)
#'        }
#'
#' **symb_choro**: displays the different modalities of a first qualitative
#' variable as symbols colored to reflect the classification of a second
#' quantitative variable.
#' \preformatted{
#' mf_map(x, var, type = "symb_choro", pal = "Mint", alpha = 1, rev = FALSE,
#'        breaks = "quantile", nbreaks, border = getOption("mapsf.fg"),
#'        pch, cex = 1, lwd = 0.7, pch_na = 4, cex_na = 1, col_na = "white",
#'        val_order,
#'        leg_pos = mf_get_leg_pos(x, 1), leg_title = var,
#'        leg_title_cex = c(0.8, 0.8), leg_val_cex = c(0.6, 0.6),
#'        leg_val_rnd = 2, leg_no_data = c("No data", "No data"),
#'        leg_frame = c(FALSE, FALSE), leg_frame_border = getOption("mapsf.fg"),
#'        leg_horiz = FALSE, leg_adj = c(0, 0), leg_fg = getOption("mapsf.fg"),
#'        leg_bg = getOption("mapsf.bg"), leg_size = 1,
#'        leg_box_border = getOption("mapsf.fg"), leg_box_cex = c(1, 1),
#'        add = TRUE)
#'        }
#'
#' ## Breaks limits
#' Breaks defined by a numeric vector or a classification method are
#' left-closed: breaks defined by \code{c(2, 5, 10, 15, 20)}
#' will be mapped as [2 - 5[, [5 - 10[, [10 - 15[, \[15 - 20].
#' The "jenks" method is an exception and has to be right-closed.
#' Jenks breaks computed as \code{c(2, 5, 10, 15, 20)}
#' will be mapped as \[2 - 5], ]5 - 10], ]10 - 15], ]15 - 20].
#'
#'
#'
#' @eval my_params(c(
#' "xfull",
#' "var",
#' "pal",
#' "alpha",
#' "rev",
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
#' 'val_order',
#' 'leg_adj',
#' 'leg_horiz',
#' 'leg_size',
#' 'leg_border',
#' 'leg_box_border',
#' 'leg_box_cex',
#' 'leg_fg',
#' 'leg_bg',
#' 'leg_frame_border'))
#' @param ... further parameters from \link{plot} for sfc objects
#' @param type
#' * **base**: base maps
#' * **prop**: proportional symbols maps
#' * **choro**: choropleth maps
#' * **typo**: typology maps
#' * **symb**: symbols maps
#' * **grad**: graduated symbols maps
#' * **prop_choro**: proportional symbols maps with symbols colors based
#' on a quantitative data classification
#' * **prop_typo**: proportional symbols maps with symbols colors based
#' on qualitative data
#' * **symb_choro**: symbols maps with symbols colors based on
#' a quantitative data classification
#' @param cex point size
#' @param pch point type
#' @param expandBB fractional values to expand the bounding box with, in each
#' direction (bottom, left, top, right)
#'
#'
#' @export
#'
#'
#' @md
#'
#'
#' @return x is (invisibly) returned.
#'
#'
#' @examples
#' library(mapsf)
#' mtq <- mf_get_mtq()
#' # basic examples
#' # type = "base"
#' mf_map(mtq)
#' # type = "prop"
#' mf_map(mtq)
#' mf_map(mtq, var = "POP", type = "prop")
#' # type = "choro"
#' mf_map(mtq, var = "MED", type = "choro")
#' # type = "typo"
#' mf_map(mtq, "STATUS", "typo")
#' # type = "symb"
#' mf_map(mtq)
#' mf_map(mtq, "STATUS", "symb")
#' # type = "grad"
#' mf_map(mtq)
#' mf_map(mtq, var = "POP", type = "grad")
#' # type = "prop_choro"
#' mf_map(mtq)
#' mf_map(mtq, var = c("POP", "MED"), type = "prop_choro")
#' # type = "prop_typo"
#' mf_map(mtq)
#' mf_map(mtq, var = c("POP", "STATUS"), type = "prop_typo")
#' # type = "symb_choro
#' mf_map(mtq)
#' mf_map(mtq, var = c("STATUS", "MED"), type = "symb_choro")
#'
#'
#'
#'
#' # detailed examples
#' # type = "base"
#' mf_map(mtq, type = "base", col = "lightblue", lwd = 1.5, lty = 2)
#'
#' # type = "prop"
#' mf_map(mtq)
#' mf_map(
#'   x = mtq, var = "POP", type = "prop",
#'   inches = .4, symbol = "circle", val_max = 90000,
#'   col = "lightblue", border = "grey", lwd = 1,
#'   leg_pos = "right", leg_title = "Population",
#'   leg_title_cex = 1, leg_val_cex = .8, leg_val_rnd = 0,
#'   leg_frame = TRUE, add = TRUE
#' )
#'
#' # type = "choro"
#' mtq[6, "MED"] <- NA
#' mf_map(
#'   x = mtq, var = "MED", type = "choro",
#'   col_na = "grey80", pal = "Cividis",
#'   breaks = "quantile", nbreaks = 4, border = "white",
#'   lwd = .5, leg_pos = "topleft",
#'   leg_title = "Median Income", leg_title_cex = 1.1,
#'   leg_val_cex = 1, leg_val_rnd = -2, leg_no_data = "No data",
#'   leg_frame = TRUE, leg_adj = c(0, -3)
#' )
#'
#' # type = "typo"
#' mtq[4, "STATUS"] <- NA
#' mf_map(
#'   x = mtq, var = "STATUS", type = "typo",
#'   pal = c("red", "blue", "yellow"), lwd = 1.1,
#'   val_order = c("Prefecture", "Sub-prefecture", "Simple municipality"),
#'   col_na = "green", border = "brown",
#'   leg_pos = "bottomleft",
#'   leg_title = "Status", leg_title_cex = 1.1,
#'   leg_val_cex = 1, leg_no_data = "No data",
#'   leg_frame = TRUE, add = FALSE
#' )
#'
#' # type = "symb"
#' mf_map(mtq)
#' mf_map(
#'   x = mtq, var = "STATUS", type = "symb",
#'   pch = c(21:23), pal = c("red1", "tan1", "khaki1"),
#'   border = "grey20", cex = c(2, 1.5, 1), lwd = .5,
#'   val_order = c("Prefecture", "Sub-prefecture", "Simple municipality"),
#'   pch_na = 24, col_na = "blue", leg_frame = TRUE
#' )
#'
#' # type = "grad"
#' mf_map(mtq)
#' mf_map(mtq, var = "POP", type = "grad", pch = 22,
#'        breaks = "quantile", nbreaks = 4, lwd = 2, border = "blue",
#'        cex = c(.75,1.5,3,5), col = "lightgreen")
#'
#' # type = "prop_choro"
#' mf_map(mtq)
#' mf_map(
#'   x = mtq, var = c("POP", "MED"), type = "prop_choro",
#'   inches = .35, border = "tomato4",
#'   val_max = 90000, symbol = "circle", col_na = "white", pal = "Cividis",
#'   breaks = "equal", nbreaks = 4, lwd = 4,
#'   leg_pos =  "bottomleft",
#'   leg_title = c("Population", "Median Income"),
#'   leg_title_cex = c(0.8, 1),
#'   leg_val_cex = c(.7, .9),
#'   leg_val_rnd = c(0, 0),
#'   leg_no_data = "No data",
#'   leg_frame = c(TRUE, TRUE),
#'   add = TRUE
#' )
#'
#' # type = "prop_typo"
#' mf_map(mtq)
#' mf_map(
#'   x = mtq, var = c("POP", "STATUS"), type = "prop_typo",
#'   inches = .35, border = "tomato4",
#'   val_max = 90000, symbol = "circle", col_na = "white", pal = "Dynamic",
#'   lwd = 2,
#'   leg_pos = c("bottomright", "bottomleft"),
#'   leg_title = c("Population", "Municipality\nstatus"),
#'   leg_title_cex = c(0.9, 0.9),
#'   leg_val_cex = c(.7, .7),
#'   val_order = c("Prefecture", "Sub-prefecture", "Simple municipality"),
#'   leg_no_data = "No dada",
#'   leg_frame = c(TRUE, TRUE),
#'   add = TRUE
#' )
#'
#' # type = "symb_choro"
#' mf_map(mtq)
#' mf_map(
#'   mtq, c("STATUS", "MED"), type = "symb_choro",
#'   pal = "Reds 3", breaks = "quantile", nbreaks = 4,
#'   pch = 21:23, cex = c(3, 2, 1),
#'   pch_na = 25, cex_na = 1.5, col_na = "blue",
#'   val_order = c(
#'     "Prefecture",
#'     "Sub-prefecture",
#'     "Simple municipality"
#'   )
#' )
mf_map <- function(x,
                   var,
                   type = "base",
                   breaks,
                   nbreaks,
                   pal,
                   alpha,
                   rev,
                   inches,
                   val_max,
                   symbol,
                   col,
                   lwd_max,
                   val_order,
                   pch,
                   cex,
                   border,
                   lwd,
                   col_na,
                   cex_na,
                   pch_na,
                   expandBB,
                   add,
                   leg_pos,
                   leg_title,
                   leg_title_cex,
                   leg_val_cex,
                   leg_val_rnd,
                   leg_no_data,
                   leg_frame,
                   leg_frame_border,
                   leg_horiz,
                   leg_adj,
                   leg_bg,
                   leg_fg,
                   leg_size,
                   leg_border,
                   leg_box_border,
                   leg_box_cex,
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
  if (cl[1] == FALSE && cl[2] == TRUE && type != "base") {
    stop(paste0("'x' should be an sf object."), call. = FALSE)
  }
  if (cl[1] == FALSE && cl[2] == FALSE && cl[3] == FALSE) {
    stop(paste0("'x' should be an sf, sfc or sfg object."), call. = FALSE)
  }

  if (!missing(var)) {
    if (type == "base") {
      message("Please use the 'type' argument to map variables.")
    } else {
      lv <- length(var)
      lin <- var %in% names(x)
      if (lv != length(lin[lin == TRUE])) {
        stop(
          paste0("It is likely that 'var' is not a valid variable name."),
          call. = FALSE
        )
      }
    }
  }

  # add mgmgt, set default add, do not add if no device is launch
  if (missing(add)) {
    add <- switch(
      type,
      prop = TRUE,
      choro = FALSE,
      typo = FALSE,
      symb = TRUE,
      base = FALSE,
      grad = TRUE,
      prop_choro = TRUE,
      prop_typo = TRUE,
      symb_choro = TRUE
    )
  }


  if (is.null(grDevices::dev.list())) {
    add <- FALSE
  }

  argx <- as.list(match.call()[-1])
  argx <- argx[!names(argx) %in% c("type", "expandBB")]


  if (type != "base") {
    argx <- check_args(argx, type)
  }

  # enabling pipe without side effect
  argx$x <- eval(x)

  if (!missing(expandBB) && !add) {
    mf_init(argx$x, expandBB = expandBB)
    argx$add <- TRUE
  } else {
    argx$add <- add
  }

  do.call(what = get(paste0("mf_", type)), argx, envir = parent.frame())

  return(invisible(x))
}

