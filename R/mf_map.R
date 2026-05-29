#' @title Plot a map
#' @description
#' `mf_map()` is the main function of the package, it displays map layers on a
#' georeferenced plot.
#'
#' `mf_map()` has three main arguments:
#' * `x`, an sf object,
#' * `var`, the name(s) of a variable(s) to map,
#' * `type`, the map type.
#'
#' Relevant arguments and default values are different for each map type and are
#' described in dedicated help pages (see [base][mf_map_base],
#' [choro][mf_map_choro], [typo][mf_map_typo], [prop][mf_map_prop],
#' [prop_choro][mf_map_prop_choro], [prop_typo][mf_map_prop_typo],
#' [symb][mf_map_symb], [grad][mf_map_grad] or [symb_choro][mf_map_symb_choro]).
#' @family map types
#' @usage
#' mf_map(x, var, type = "base",
#'        breaks, nbreaks, pal, alpha, rev, inches, val_max, symbol, col,
#'        lwd_max, val_order, pch, cex, border, lwd, col_na, cex_na, pch_na,
#'        expandBB, extent, bg, add,
#'        leg_pos, leg_title, leg_title_cex, leg_val_cex, leg_val_rnd,
#'        leg_val_dec, leg_val_big, leg_no_data, leg_frame, leg_frame_border,
#'        leg_horiz, leg_adj, leg_bg, leg_fg, leg_size,
#'        leg_box_border, leg_box_cex, ...)
#' @param x object of class `sf`
#' @param var name(s) of the variable(s) to map
#' @param type
#' * **[base][mf_map_base]**: base maps
#' * **[choro][mf_map_choro]**: choropleth maps
#' * **[typo][mf_map_typo]**: typology maps
#' * **[prop][mf_map_prop]**: proportional symbols maps
#' * **[prop_choro][mf_map_prop_choro]**: proportional symbols with choropleth
#' coloration
#' * **[prop_typo][mf_map_prop_typo]**: proportional symbols with typology
#' coloration
#' * **[symb][mf_map_symb]**: symbols maps
#' * **[grad][mf_map_grad]**: graduated symbols maps
#' * **[symb_choro][mf_map_symb_choro]**: symbols with choropleth coloration
#' @param breaks	either a numeric vector with the actual breaks, or a
#' classification method name. The main methods are 'quantile',
#' 'equal', 'msd', 'ckmeans' (natural breaks), 'Q6' and 'geom'.
#' See [mf_get_breaks] for details.
#' @param nbreaks number of classes
#' @param pal a set of colors (hex codes) or a palette name. Palette names can
#' be obtained with [hcl.pals].
#' @param alpha `col` or `pal` opacity, in the range \[0,1\] (0 means
#' transparent and 1 means opaque). Default is set to 1.
#' @param rev if `pal` is a palette name, whether the ordering of the colors
#' should be reversed (TRUE) or not (FALSE)
#' @param inches size of the largest symbol in inches (radius for circles,
#' half width for squares)
#' @param val_max maximum value corresponding to the largest symbol or line
#' @param lwd_max width of the largest line
#' @param val_order modalities order in the legend, a character vector that
#' matches `var` modalities
#' @param symbol type of proportional symbols, either "circle" or "square"
#' @param border border color for polygons or symbols. It can be a hex
#' code or a color name given by [colors].
#' @param lwd border width of polygons, symbols or lines
#' @param pch type of symbol to use for points, see [pch]
#' @param cex symbols size, 2 means 2 times bigger
#' @param expandBB expansion of the map area in each direction (bottom, left,
#' top, right). The expansion is expressed as a share of `x` width
#' (for left and right values) or a share of `x` height (for bottom and top
#' values).
#' @param extent `sf` object used to define the map extent; defaults to `x`.
#' `extent` and `x` must use the same CRS.
#' @param bg background color of the map, hex code or color name given by
#' [colors], ignored if `add = TRUE`
#' @param add whether to add the layer to an existing plot (TRUE) or not (FALSE)
#' @param col a color, hex code or color name given by [colors]
#' @param pch_na symbol to use for missing values on points, see [pch]
#' @param cex_na symbols size for missing values on points
#' @param col_na color for missing values, a hex code or
#' a color name given by [colors].
#' @param leg_pos position of the legend, one of 'topleft', 'top','topright',
#' 'right', 'bottomright', 'bottom', 'bottomleft', 'left' or a vector of two
#' coordinates in map units (c(x, y)). Use `NA` to avoid plotting the legend,
#' use 'interactive' to choose the legend position by clicking on the map.
#' @param leg_title legend title
#' @param leg_title_cex size of the title
#' @param leg_val_cex size of the values
#' @param leg_val_rnd number of decimal places of the values displayed in the
#' legend
#' @param leg_val_dec	decimal separator
#' @param leg_val_big	thousands separator
#' @param leg_no_data label for missing values
#' @param leg_frame	whether to add a frame to the legend (TRUE) or not (FALSE)
#' @param leg_frame_border border color of the legend frame
#' @param leg_horiz	display the legend horizontally (for proportional symbols
#' and choropleth types)
#' @param leg_adj	adjust the position of the legend in x and y directions
#' @param leg_bg color of the legend background
#' @param leg_fg	color of the legend foreground
#' @param leg_size size of the legend. Combine this argument with
#' `leg_title_cex` and `leg_val_cex`.
#' @param leg_box_border border color of legend boxes (for types related to
#' choropleth and typology)
#' @param leg_box_cex	width and height size expansion of boxes
#' @param ... ignored
#'
#' @export
#' @return x is (invisibly) returned.
#' @examples
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
                   extent,
                   bg,
                   add,
                   leg_pos,
                   leg_title,
                   leg_title_cex,
                   leg_val_cex,
                   leg_val_rnd,
                   leg_val_dec,
                   leg_val_big,
                   leg_no_data,
                   leg_frame,
                   leg_frame_border,
                   leg_horiz,
                   leg_adj,
                   leg_bg,
                   leg_fg,
                   leg_size,
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
    add <- switch(type,
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


  if (is.null(dev.list())) {
    add <- FALSE
  }

  argx <- as.list(match.call()[-1])
  argx <- argx[!names(argx) %in% c("type")]


  if (type != "base") {
    argx <- check_args(argx, type)
  }

  # enabling pipe without side effect
  argx$x <- eval(x)

  do.call(what = get(paste0("mf_", type)), argx, envir = parent.frame())

  return(invisible(x))
}
