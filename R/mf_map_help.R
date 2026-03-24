# base ----
#' @name mf_map_base
#' @title Plot a base map
#' @description
#' `mf_map()` can be used to display geographic layers (`sf` objects), using
#' the default map type **base**.
#'
#' ## Usage
#' For polygons:
#' ```r
#' mf_map(x, col, border, lwd = 0.7, lty = 1,
#'        alpha, expandBB, extent, bg, add = FALSE)
#' ```
#' For points:
#' ```r
#' mf_map(x, col, border, pch = 20, cex = 1, lwd = 0.7,
#'        alpha, expandBB, extent, bg, add = FALSE)
#' ```
#' For lines:
#' ```r
#' mf_map(x, col, lwd = .7, lty = 1,
#'        alpha, expandBB, extent, bg, add = FALSE)
#' ```
#' @param x	object of class `sf`, `sfc` or `sfg`
#' @param col a color, hex code or color name given by [colors].
#' The default color for polygons is the foreground color, the default color for
#' points and lines the highlight color (see [mf_get_theme_value]).
#' @param border border color for polygons and points symbols, hex code or
#' color name given by [colors]. The default color for polygon is the
#' highlight color, the default color for points is the foreground color (see
#' [mf_get_theme_value]).
#' @param lwd border width for polygons and points symbols, lines width
#' @param lty type of line for polygons borders and lines
#' @param pch type of symbol to use for points, see [pch]
#' @param cex symbols size, 2 means 2 times bigger
#' @param alpha,expandBB,extent,bg,add arguments described in [mf_map]
#' @return x is (invisibly) returned.
#' @seealso [mf_map()]
#' @examples
#' mtq <- mf_get_mtq()
#' pts <- mf_get_mtq("points")
#' flows <- mf_get_mtq("lines")
#' mf_map(mtq, lty = 3)
#' mf_map(pts, col = "red", border = "white", pch = 21, add = TRUE)
#' mf_map(flows, col = "coral", lwd = 2, add = TRUE)
NULL

# prop ----
#' @name mf_map_prop
#' @title Plot proportional symbols
#' @description
#' With the **prop** map type, `mf_map()` displays symbols (squares or circles)
#' with areas proportional to a quantitative variable (stocks).
#'
#' For polygons, centroids are used to plot proportional symbols.
#'
#' ## Usage
#' For polygons and points:
#' ```r
#' mf_map(x, var, type = "prop",
#'        inches = 0.3, val_max, symbol, col, border, lwd,
#'        expandBB, extent, bg, alpha, add = FALSE, leg_*)
#'
#' ```
#' For lines:
#' ```r
#' mf_map(x, var, type = "prop",
#'        val_max, lwd_max = 20, col,
#'        expandBB, extent, bg, alpha, add = FALSE, leg_*)
#' ```
#' @param x	object of class `sf`
#' @param var name of the variable to map
#' @param type "prop"
#' @param inches size of the largest symbol in inches (radius for circles,
#' half width for squares)
#' @param val_max maximum value corresponding to the largest symbol or line
#' @param lwd_max width of the largest line
#' @param symbol type of proportional symbols, either "circle" or "square"
#' @param col color of the proportional symbols or lines, a hex code or
#' a color name given by [colors].
#' The default color is the highlight color (see [mf_get_theme_value]).
#' @param border border color for proportional symbols, a hex code or
#' color name given by [colors]. The default color the background color (see
#' [mf_get_theme_value]).
#' @param lwd border width of proportional symbols
#' @param alpha,expandBB,extent,bg,add arguments described in [mf_map]
#' @param leg_* legend arguments described in [mf_map]
#' @return x is (invisibly) returned.
#' @seealso [mf_map()]
#' @examples
#' mtq <- mf_get_mtq()
#' flows <- mf_get_mtq("lines")
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_map(mtq, "POP", "prop",
#'   inches = .4, leg_title = "Population",
#'   leg_pos = "topright"
#' )
#' mf_map(flows, "fij", "prop",
#'   lwd_max = 10, col = "steelblue2",
#'   leg_pos = "right"
#' )
NULL

# choro ----
#' @name mf_map_choro
#' @title Plot a choropleth map
#' @description
#' With the **choro** map type, `mf_map()` displays a choropleth map.
#'
#' In choropleth maps, areas are shaded according to the variation of a
#' quantitative variable. They are used to represent ratios or indices.
#'
#' ## Usage
#' For polygons:
#' ```r
#' mf_map(x, var, type = "choro",
#'        breaks = "quantile", nbreaks, pal, rev = FALSE,
#'        border, lwd = 0.7, col_na = "white",
#'        alpha, expandBB, extent, bg, add = FALSE, leg_*)
#' ```
#' For points:
#' ```r
#' mf_map(x, var, type = "choro",
#'        breaks = "quantile", nbreaks, pal, rev = FALSE,
#'        border, pch = 21, cex = 2, lwd = 0.7, col_na = "white",
#'        alpha, expandBB, extent, bg, add = FALSE, leg_*)
#' ```
#' For lines:
#' ```r
#' mf_map(x, var, type = "choro",
#'        breaks = "quantile", nbreaks, pal, rev = FALSE, lwd = .7,
#'        col_na = "white",
#'        alpha, expandBB, extent, bg, add = FALSE, leg_*)
#' ```
#' @param x	object of class `sf`
#' @param var name of the variable to map
#' @param type "choro"
#' @param breaks	either a numeric vector with the actual breaks, or a
#' classification method name. The main methods are 'quantile',
#' 'equal', 'msd', 'ckmeans' (natural breaks), 'Q6' and 'geom'.
#' See [mf_get_breaks] for details.
#' @param nbreaks number of classes
#' @param pal a set of colors (hex codes) or a palette name. Palette names can
#' be obtained with [hcl.pals]. The default palette is the pal_seq palette
#' (see [mf_get_theme_value]).
#' @param rev if `pal` is a palette name, whether the ordering of the colors
#' should be reversed (TRUE) or not (FALSE)
#' @param border border color for polygons and symbols, a hex code or
#' color name given by [colors]. The default color for polygons is the
#' highlight color, the default color for points is the background color (see
#' [mf_get_theme_value]).
#' @param lwd border width for polygons and points symbols, lines width
#' @param pch type of symbol to use for points, see [pch] (points only)
#' @param cex symbols size, 2 means 2 times bigger (points only)
#' @param col_na color for missing values, a hex code or
#' a color name given by [colors].
#' @param alpha,expandBB,extent,bg,add arguments described in [mf_map]
#' @param leg_* legend arguments described in [mf_map]
#' @return x is (invisibly) returned.
#' @seealso [mf_map()], [mf_distr()], [mf_get_breaks()], [mf_get_pal()]
#' @examples
#' mtq <- mf_get_mtq()
#' pts <- mf_get_mtq("points")
#' flows <- mf_get_mtq("lines")
#' # polygons
#' mtq[6, "MED"] <- NA
#' mf_map(
#'   x = mtq, var = "MED", type = "choro",
#'   col_na = "grey90", pal = "Cividis",
#'   breaks = "equal", nbreaks = 5, border = "white",
#'   lwd = .5, leg_pos = "topleft",
#'   leg_title = "Median Income", leg_title_cex = 1,
#'   leg_val_cex = .9, leg_val_rnd = -2, leg_no_data = "No data",
#'   leg_box_cex = c(0.5, 3), leg_box_border = NA, leg_frame = FALSE
#' )
#' # points
#' mf_map(
#'   x = pts, var = "MED", type = "choro",
#'   cex = 5, pal = "Burg", border = "black",
#'   leg_horiz = TRUE, leg_val_big = " ",
#'   leg_val_rnd = -2, leg_pos = "bottomleft"
#' )
#' # lines
#' mf_map(mtq, extent = flows)
#' mf_map(
#'   x = flows, var = "fij", type = "choro",
#'   breaks = "equal", nbreaks = 3, add = TRUE,
#'   lwd = 5, pal = "Burg", leg_horiz = TRUE,
#'   leg_box_cex = c(.7, 1),
#'   leg_val_rnd = 0, leg_pos = "bottomleft"
#' )
NULL

# typo ----
#' @name mf_map_typo
#' @title Plot a typology map
#' @description
#' With the **typo** map type, `mf_map()` displays a typology map.
#'
#' In typology maps, areas are shaded according to the modalities of a
#' qualitative variable.
#'
#' ## Usage
#' For polygons:
#' ```r
#' mf_map(x, var, type = "typo",
#'        pal, rev = FALSE, val_order,
#'        border, lwd = 0.7, col_na = "white",
#'        alpha, expandBB, extent, bg, add = FALSE, leg_*)
#' ```
#' For points:
#' ```r
#' mf_map(x, var, type = "typo",
#'        pal, rev = FALSE, val_order,
#'        border, pch = 21, cex = 2, lwd = 0.7, col_na = "white",
#'        alpha, expandBB, extent, bg, add = FALSE, leg_*)
#' ```
#' For lines:
#' ```r
#' mf_map(x, var, type = "typo",
#'        pal, rev = FALSE, val_order, lwd = .7,
#'        col_na = "white",
#'        alpha, expandBB, extent, bg, add = FALSE, leg_*)
#' ```
#' @param x	object of class `sf`
#' @param var name of the variable to map
#' @param type "choro"
#' @param pal a set of colors (hex codes) or a palette name. Palette names can
#' be obtained with [hcl.pals]. The default palette is the pal_quali palette
#' (see [mf_get_theme_value]).
#' @param rev if `pal` is a palette name, whether the ordering of the colors
#' should be reversed (TRUE) or not (FALSE)
#' @param val_order modalities order in the legend, a character vector that
#' matches `var` modalities. Default to alphabetic order of modalities.
#' @param border border color for polygons and symbols, a hex code or
#' color name given by [colors]. The default color for polygons is the
#' highlight color, the default color for points is the background color (see
#' [mf_get_theme_value]).
#' @param lwd border width for polygons and points symbols, lines width
#' @param pch type of symbol to use for points, see [pch] (points only)
#' @param cex symbols size, 2 means 2 times bigger (points only)
#' @param col_na color for missing values, a hex code or
#' a color name given by [colors].
#' @param alpha,expandBB,extent,bg,add arguments described in [mf_map]
#' @param leg_* legend arguments described in [mf_map]
#' @return x is (invisibly) returned.
#' @seealso [mf_map()], [mf_get_pal()]
#' @examples
#' mtq <- mf_get_mtq()
#' pts <- mf_get_mtq("points")
#' flows <- mf_get_mtq("lines")
#' # polygons
#' mtq[6, "STATUS"] <- NA
#' mf_map(
#'   x = mtq, var = "STATUS", type = "typo",
#'   col_na = "grey90", border = "white",
#'   pal = c("#FFE93F", "#00214E", "#7C7C7C"),
#'   val_order = c("Prefecture", "Sub-prefecture", "Simple municipality"),
#'   lwd = .5, leg_pos = "bottomleft",
#'   leg_title = "", leg_title_cex = 1,
#'   leg_val_cex = .9, leg_no_data = "No data",
#'   leg_box_cex = c(0.5, 3), leg_box_border = NA
#' )
#' # points
#' mf_map(
#'   x = pts, var = "STATUS", type = "typo",
#'   cex = 3, pal = "Dark 3", border = "grey",
#'   leg_pos = "bottomleft"
#' )
#' # lines
#' mf_map(mtq, extent = flows)
#' mf_map(
#'   x = flows, var = "sj", type = "typo",
#'   add = TRUE,
#'   lwd = 2, pal = c("red", "blue"),
#'   leg_pos = "bottomleft"
#' )
NULL

# grad ----
#' @name mf_map_grad
#' @title Plot graduated symbols
#' @description
#' With the **grad** map type, `mf_map()` displays graduated symbols on a map.
#'
#' Graduated symbols are based on classified quantitative variables.
#'
#' For polygons, centroids are used to plot graduated symbols.
#' ## Usage
#' For polygons and points:
#' ```r
#' mf_map(x, var, type = "grad",
#'        breaks = "quantile", nbreaks = 3,
#'        col, border, lwd = 0.7, pch = 21, cex,
#'        alpha, expandBB, extent, bg, add = TRUE, leg_*)
#' ```
#' For lines:
#' ```r
#' mf_map(x, var, type = "grad",
#'        breaks = "quantile", nbreaks = 3,
#'        col, lwd,
#'        alpha, expandBB, extent, bg, add = TRUE, leg_*)
#' ```
#' @param x	object of class `sf`
#' @param var name of the variable to map
#' @param type "grad"
#' @param breaks	either a numeric vector with the actual breaks, or a
#' classification method name. The main methods are 'quantile',
#' 'equal', 'msd', 'ckmeans' (natural breaks), 'Q6' and 'geom'.
#' See [mf_get_breaks] for details.
#' @param nbreaks number of classes
#' @param col color of the graduated symbols or lines, a hex code or
#' a color name given by [colors].
#' The default color is the highlight color (see [mf_get_theme_value]).
#' @param border border color for symbols, a hex code or
#' color name given by [colors]. The default color is the background color (see
#' [mf_get_theme_value]).
#' @param lwd border width for graduated symbols, a vector of line widths for
#' graduated lines
#' @param pch type of symbol to use for points, see [pch] (points only)
#' @param cex a vector of sizes for symbols (points only)
#' @param alpha,expandBB,extent,bg,add arguments described in [mf_map]
#' @param leg_* legend arguments described in [mf_map]
#' @return x is (invisibly) returned.
#' @seealso [mf_map()]
#' @examples
#' mtq <- mf_get_mtq()
#' flows <- mf_get_mtq("lines")
#' mf_map(mtq, bg = "cornsilk2")
#' mf_map(flows, "fij", "grad",
#'   breaks = "geom", nbreaks = 3,
#'   lwd = c(1, 3, 7),
#'   leg_title = "N. commuters",
#'   leg_pos = "bottomleft", leg_val_rnd = 0
#' )
#' mf_map(mtq, "POP", "grad",
#'   breaks = c(686, 5000, 25000, 82502),
#'   cex = c(1, 2, 4), pch = 22, col = "steelblue",
#'   leg_title = "Population", leg_pos = "topright",
#'   leg_frame = TRUE
#' )
NULL

# symb ----
#' @name mf_map_symb
#' @title Plot symbols
#' @description
#' `mf_map()` can use symbols to display qualitative data, using **symb**
#' map type.
#'
#' For polygons, centroids are used to plot graduated symbols. This map type
#' is not available for lines.
#'
#' ## Usage
#' For polygons and points:
#' ```r
#' mf_map(x, var, type = "symb",
#'        pch, cex = 2, lwd = 0.7, pal, rev = FALSE, border,
#'        val_order,
#'        col_na = "grey", pch_na = 4, cex_na = 1,
#'        alpha, expandBB, extent, bg, add = TRUE, leg_*)
#' ```
#' @param x	object of class `sf` (polygons or points)
#' @param var name of the variable to map
#' @param type "symb"
#' @param pch a vector of types of symbols, see [pch].
#' The length of `pch` should match the number of modalities.
#' @param cex a vector of sizes for symbols.
#' The length of `cex` should match the number of modalities.
#' @param lwd border width of symbols
#' @param pal a set of colors (hex codes) or a palette name. Palette names can
#' be obtained with [hcl.pals]. The default palette is the pal_quali palette
#' (see [mf_get_theme_value]).
#' @param rev if `pal` is a palette name, whether the ordering of the colors
#' should be reversed (TRUE) or not (FALSE)
#' @param border border color for symbols, a hex code or color name given
#' by [colors]. The default color is the background color (see
#' [mf_get_theme_value]).
#' @param val_order modalities order in the legend, a character vector that
#' matches `var` modalities. Default to alphabetic order of modalities.
#' @param pch_na type of symbol for missing values, see [pch]
#' @param cex_na size of symbol for missing values
#' @param col_na color for missing values, a hex code or
#' a color name given by [colors]
#' @return x is (invisibly) returned.
#' @examples
#' mtq <- mf_get_mtq()
#' mtq$STATUS[3] <- NA
#' mf_map(mtq)
#' mf_map(mtq, "STATUS", "symb",
#'   pal = "Berlin", border = "white", lwd = 1,
#'   cex = c(4, 3, 2), pch = c(21:23), col_na = "red",
#'   val_order = c("Prefecture", "Sub-prefecture", "Simple municipality"),
#'   leg_title = ""
#' )
NULL


# prop_choro ----
#' @name mf_map_prop_choro
#' @title Plot proportional symbols with choropleth coloration
#' @description
#' `mf_map()` with **prop_choro** type creates symbols that are proportional
#' to values of a first variable and colored to reflect the classification of a
#' second variable.
#'
#' This map types uses two variables and some arguments need to be set for both
#' variables (see Details).
#'
#' For polygons, centroids are used to plot proportional symbols. This map type
#' is not available for lines.
#'
#' ## Usage
#' For polygons and points:
#' ```r
#' mf_map(x, var, type = "prop_choro",
#'        inches = 0.3, val_max,  symbol = "circle",
#'        pal, rev = FALSE, breaks = "quantile", nbreaks,
#'        border, lwd = 0.7, col_na = "white",
#'        alpha, expandBB, extent, bg, add = TRUE, leg_*)
#'
#'
#'
#' @param x	object of class `sf` (polygons or points)
#' @param var names of the variables to map. The first value refers to the
#' proportional symbols, the second one to the choropleth coloration.
#' @param type "prop_choro"
#' @param inches size of the largest symbol in inches (radius for circles,
#' half width for squares)
#' @param val_max maximum value corresponding to the largest symbol or line
#' @param symbol type of proportional symbols, either "circle" or "square"
#' @param breaks	either a numeric vector with the actual breaks, or a
#' classification method name. The main methods are 'quantile',
#' 'equal', 'msd', 'ckmeans' (natural breaks), 'Q6' and 'geom'.
#' See [mf_get_breaks] for details.
#' @param nbreaks number of classes
#' @param pal a set of colors (hex codes) or a palette name. Palette names can
#' be obtained with [hcl.pals]. The default palette is the pal_seq palette
#' (see [mf_get_theme_value]).
#' @param rev if `pal` is a palette name, whether the ordering of the colors
#' should be reversed (TRUE) or not (FALSE)
#' @param border border color of proportional symbols, a hex code or
#' color name given by [colors]. The default color is the background color (see
#' [mf_get_theme_value]).
#' @param lwd border width of proportional symbols
#' @param col_na color for missing values, a hex code or
#' a color name given by [colors]
#' @param alpha,expandBB,extent,bg,add arguments described in [mf_map]
#' @param leg_* legend arguments described in [mf_map]. See details for
#' arguments with two values.
#' @return x is (invisibly) returned.
#' @details
#' Legend arguments that need two values are: 'leg_title',
#' 'leg_val_rnd', and 'leg_horiz'. The first
#' values refers to the proportional symbols legend, the second one to the
#' choropleth legend.
#'
#'
#' @seealso [mf_map()], [mf_map_prop], [mf_map_choro], [mf_distr()],
#' [mf_get_breaks()], [mf_get_pal()]
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mtq[6, "MED"] <- NA
#' mf_map(
#'   x = mtq, var = c("POP", "MED"), type = "prop_choro",
#'   inches = .2,
#'   val_max = 90000, symbol = "circle",
#'   col_na = "grey90", pal = "Cividis",
#'   breaks = "msd", nbreaks = 4, lwd = 1,
#'   leg_pos = "topright",
#'   leg_title = c("Population", "Median Income"),
#'   leg_val_rnd = c(0, 1),
#'   leg_horiz = c(TRUE, FALSE),
#'   leg_title_cex = .9,
#'   leg_val_dec = ",",
#'   leg_val_cex = .8,
#'   leg_size = 1,
#'   add = TRUE
#' )
NULL

# symb-choro ----
#' @name mf_map_symb_choro
#' @title Plot symbols with choropleth coloration
#' @description
#' `mf_map()` with **symb_choro** type creates symbols that reflect modalities
#' of a first qualitative variable and colored to reflect the classification of
#' a second variable.
#'
#' This map types uses two variables and some arguments need to be set for both
#' variables (see Details).
#'
#' For polygons, centroids are used to plot symbols. This map type
#' is not available for lines.
#'
#' ## Usage
#' For polygons and points:
#' ```r
#' mf_map(x, var, type = "symb_choro",
#'        pch, cex = 2, lwd = 0.7, border, val_order,
#'        pal, rev = FALSE, breaks = "quantile", nbreaks,
#'        pch_na = 4, cex_na = 1, col_na = "white",
#'        alpha, expandBB, extent, bg, add = TRUE, leg_*)
#' ```
#' @param x	object of class `sf` (polygons or points)
#' @param var names of the variables to map. The first value refers to the
#' symbols categories, the second one to the choropleth coloration.
#' @param type "symb_choro"
#' @param pch a vector of types of symbols, see [pch].
#' The length of `pch` should match the number of modalities.
#' @param cex a vector of sizes for symbols.
#' The length of `cex` should match the number of modalities.
#' @param lwd border width of symbols
#' @param border border color for symbols, a hex code or color name given
#' by [colors]. The default color is the background color (see
#' [mf_get_theme_value]).
#' @param val_order modalities order in the legend, a character vector that
#' matches `var` modalities. Default to alphabetic order of modalities.
#' @param pal a set of colors (hex codes) or a palette name. Palette names can
#' be obtained with [hcl.pals]. The default palette is the pal_seq palette
#' (see [mf_get_theme_value]).
#' @param rev if `pal` is a palette name, whether the ordering of the colors
#' should be reversed (TRUE) or not (FALSE)
#' @param breaks	either a numeric vector with the actual breaks, or a
#' classification method name. The main methods are 'quantile',
#' 'equal', 'msd', 'ckmeans' (natural breaks), 'Q6' and 'geom'.
#' See [mf_get_breaks] for details.
#' @param nbreaks number of classes
#' @param pch_na type of symbol for missing values, see [pch]
#' @param cex_na size of symbol for missing values
#' @param col_na color for missing values, a hex code or
#' a color name given by [colors]
#' @param alpha,expandBB,extent,bg,add arguments described in [mf_map]
#' @param leg_* legend arguments described in [mf_map]. See details for
#' arguments with two values.
#' @details
#' Legend arguments that need two values are: 'leg_title',
#' 'leg_no_data'. The first value refers to the symbols legend, the second
#' one to the choropleth legend.
#' @return x is (invisibly) returned.
#' @seealso [mf_map()], [mf_map_symb], [mf_map_choro], [mf_distr()],
#' [mf_get_breaks()], [mf_get_pal()]
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mtq$STATUS[4] <- NA
#' mf_map(mtq, c("STATUS", "MED"),
#'   type = "symb_choro", lwd = 1,
#'   pal = "Reds 3", breaks = "quantile", nbreaks = 4,
#'   cex = c(2, 1, 1), pch = c(20, 21, 23), pch_na = 22,
#'   leg_pos = "topright", border = "white",
#'   val_order = c("Prefecture", "Sub-prefecture", "Simple municipality")
#' )
NULL





# prop_typo ----
#' @name mf_map_prop_typo
#' @title Plot proportional symbols with typology coloration
#' @description
#' `mf_map()` with **prop_typo** type creates symbols that are proportional
#' to values of a first variable and colored to reflect the modalities of a
#' second qualitative variable.
#'
#' This map types uses two variables and some arguments need to be set for both
#' variables (see Details).
#'
#' For polygons, centroids are used to plot proportional symbols.
#' ## Usage
#' For polygons and points:
#' ```r
#' mf_map(x, var, type = "prop_typo",
#'        inches = 0.3, val_max, symbol, border,
#'        pal, rev = FALSE, val_order,
#'        border, lwd = 0.7, col_na = "white",
#'        alpha, expandBB, extent, bg, add = FALSE, leg_*)
#' ```
#' For lines:
#' ```r
#' mf_map(x, var, type = "typo",
#'        lwd_max = 15,
#'        pal, rev = FALSE, val_order,
#'        col_na = "white",
#'        alpha, expandBB, extent, bg, add = FALSE, leg_*)
#' ```
#' @param x	object of class `sf`
#' @param var names of the variables to map. The first value refers to the
#' proportional symbols, the second one to the typology coloration.
#' @param type "prop_typo"
#' @param inches size of the largest symbol in inches (radius for circles,
#' half width for squares)
#' @param lwd_max width of the largest line
#' @param val_max maximum value corresponding to the largest symbol or line
#' @param symbol type of proportional symbols, either "circle" or "square"
#' @param border border color for proportional symbols, a hex code or
#' color name given by [colors]. The default color is the background color (see
#' [mf_get_theme_value]).
#' @param pal a set of colors (hex codes) or a palette name. Palette names can
#' be obtained with [hcl.pals]. The default palette is the pal_quali palette
#' (see [mf_get_theme_value]).
#' @param rev if `pal` is a palette name, whether the ordering of the colors
#' should be reversed (TRUE) or not (FALSE)
#' @param val_order modalities order in the legend, a character vector that
#' matches `var` modalities. Default to alphabetic order of modalities.
#' @param col_na color for missing values, a hex code or
#' a color name given by [colors].
#' @param alpha,expandBB,extent,bg,add arguments described in [mf_map]
#' @param leg_* legend arguments described in [mf_map]. See details for
#' arguments with two values.
#'
#' @details
#' 'leg_title' needs two values. The first value refers to the symbols legend,
#' the second one to the choropleth legend.
#'
#' @return x is (invisibly) returned.
#' @seealso [mf_map()], [mf_map_prop], [mf_map_typo], [mf_get_pal()]
#' @examples
#' mtq <- mf_get_mtq()
#' flows <- mf_get_mtq("lines")
#' mf_map(mtq, extent = flows, expandBB = c(0,.5,0,0))
#' mf_map(flows, c("fij", "sj"), "prop_typo",
#'        val_order = c("Sub-prefecture", "Simple municipality"),
#'        pal = c( "steelblue", "lightblue"), lwd_max = 30,
#'        leg_pos = "topleft", leg_title = c("commuters", "destination"))
#' mf_map(
#'   x = mtq, var = c("POP", "STATUS"), type = "prop_typo",
#'   inches = .2, border = "tomato4", lwd = 1,
#'   pal = c("darkblue", "steelblue", "lightblue"),
#'   val_order = c("Prefecture", "Sub-prefecture", "Simple municipality"),
#'   leg_pos = "bottomleft",
#'   leg_title = c("Population", ""),
#'   leg_no_data = "No dada",
#'   add = TRUE
#' )
NULL



