#' @title Package description
#' @name mapsf
#' @description
#' Create and integrate thematic maps in your workflow. This package
#' helps to design various cartographic representations such as proportional
#' symbols, choropleth or typology maps. It also offers several functions to
#' display layout elements that improve the graphic presentation of maps
#' (e.g. scale bar, north arrow, title, labels). `mapsf` maps `sf` objects on
#' `base` graphics.
#'
#'
#' A "Get Started" **vignette** contains commented scripts on how to create
#' various maps: `vignette(topic = "mapsf", package = "mapsf")`
#'
#' @section Symbology:
#' These functions display cartographic layers.
#' - [mf_map()] Plot a map
#' - [mf_label()] Plot labels
#' - [mf_raster()] Plot a raster
#' - [mf_graticule()] Plot graticules
#'
#' @section Map layout:
#' These functions are dedicated to the map layout design.
#' - [mf_init()] Initialize a map with a specific extent
#' - [mf_theme()] Set a theme
#' - [mf_shadow()] Plot a shadow
#' - [mf_background()] Plot a background image
#' - [mf_annotation()] Plot an annotation
#' - [mf_arrow()] Plot a north arrow
#' - [mf_credits()] Plot credits
#' - [mf_layout()] Plot a map layout
#' - [mf_title()] Plot a title
#' - [mf_scale()] Plot a scale bar
#' - [mf_inset_on()] / [mf_inset_off()] Plot an inset
#' - [mf_worldmap()] Plot a point on a world map
#' - [mf_legend()] Plot a legend
#'
#' @section Utility functions:
#' - [mf_export()] Export a map
#' - [mf_distr()] Plot a distribution
#' - [mf_get_links()] Get a link layer from a data.frame of links
#' - [mf_get_pal()] Get color palettes
#' - [mf_get_breaks()] Get class intervals
#' - [mf_get_mtq()] Get the 'mtq' dataset
#' - [mf_get_ratio()] Get map width and height values
#
#' @md
#'
#' @docType package
"_PACKAGE"
