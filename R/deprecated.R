#' @title Deprecated functions in mapsf
#'
#' @description
#' These functions and features still work but will be removed in the next
#' major version of the package.
#'
#' ## `mf_map` sub-functions
#'
#' Instead of using the following deprecated functions, one can use [mf_map]
#' with the corresponding type:
#' - `mf_base()` => [mf_map_base]
#' - `mf_choro()` => [mf_map_choro]
#' - `mf_prop()` => [mf_map_prop]
#' - `mf_typo()` => [mf_map_typo]
#' - `mf_symb()` => [mf_map_symb]
#' - `mf_grad()` => [mf_map_grad]
#' - `mf_prop_typo()` => [mf_map_prop_typo]
#' - `mf_prop_choro()` => [mf_map_prop_choro]
#' - `mf_symb_choro()` => [mf_map_symb_choro]
#'
#'
#' ## `mf_init`
#'
#' `mf_init` is deprecated. It is possible to use `mf_map` instead
#' (`mf_map(x, type = "base", col = NA, border = NA)`.
#'
#' It is also possible to use the `extent` argument of [mf_map].
#'
#'
#' ## `mf_export`
#'
#' `mf_export` is deprecated, use [mf_png] or [mf_svg] instead.
#'
#' ## `mf_annotation`
#'
#' `mf_annotation` is deprecated, use [mf_text] instead.
#'
#' ## Double legends
#'
#' The use of separated legends for map types **prop_choro**, **prop_typo** and
#' **symb_choro** is deprecated. Use `leg_pos = NA` and [mf_legend] if
#' separated legends are needed.
#'
#' ## Theming system
#'
#' In [mf_theme], the following themes are deprecated: "default", "brutal",
#' "ink", "dark", "agolalight", "candy", "darkula", "iceberg", "green",
#' "nevermind", "jsk" and "barcelona".\cr
#' The following arguments are also deprecated: "bg", "fg", "tab",
#' "pos", "inner", "line", "cex" and "font".\cr
#'
#' Although the map theming system has been radically changed in version 1.0.0
#' of the package, you can still use the old themes by referencing them by name.
#' If you need to use the *pre* v1.0.0 default theme, set `x` to "default".\cr
#' If an old theme is set, only deprecated arguments are used and others are
#' ignored.\cr
#' If current and deprecated arguments are mixed, only deprecated arguments are
#' used and others are ignored.\cr
#' All references and usages of the old theming system will be removed in the
#' next major version.
#'
#' @name mapsf-deprecated
NULL
