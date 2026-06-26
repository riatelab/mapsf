#' @title Defunct functions in mapsf
#'
#' @description
#'
#' These functions and features were in the package once but have be removed.
#'
#' - `mf_annotation`: defunct in 2.0.0, use [mf_text] instead.
#' - `mf_export`: defunct in 2.0.0, use [mf_png] or [mf_svg] instead.
#' - `mf_init`: defunct in 2.0.0, use [mf_map] instead.
#' - `mf_base`: defunct in 2.0.0, use [mf_map] with `type = "base"` instead,
#' see [mf_map_base].
#' - `mf_choro`: defunct in 2.0.0, use [mf_map] with `type = "choro"` instead,
#' see [mf_map_choro].
#' - `mf_prop`: defunct in 2.0.0, use [mf_map] with `type = "prop"` instead,
#' see [mf_map_prop].
#' - `mf_typo`: defunct in 2.0.0, use [mf_map] with `type = "typo"` instead,
#' see [mf_map_typo].
#' - `mf_symb`: defunct in 2.0.0, use [mf_map] with `type = "symb"` instead,
#' see [mf_map_symb].
#' - `mf_grad`: defunct in 2.0.0, use [mf_map] with `type = "grad"` instead,
#' see [mf_map_grad].
#' - `mf_prop_typo`: defunct in 2.0.0, use [mf_map] with `type = "prop_typo"`
#' instead, see [mf_map_prop_typo].
#' - `mf_prop_choro`: defunct in 2.0.0, use [mf_map] with `type = "prop_choro"`
#' instead, see [mf_map_prop_choro].
#' - `mf_symb_choro`: defunct in 2.0.0, use [mf_map] with `type = "symb_choro"`
#' instead, see [mf_map_symb_choro].
#'
#' - separated legends in `mf_map()` with types "prop_choro", "prop_typo" and
#' "symb_choro": defunct in 2.0.0, use `leg_pos = NA` and calls to [mf_legend]
#' instead.
#' @name mapsf-defunct
NULL
