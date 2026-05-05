# Package description

Create and integrate thematic maps in your workflow. This package helps
to design various cartographic representations such as proportional
symbols, choropleth or typology maps. It also offers several functions
to display layout elements that improve the graphic presentation of maps
(e.g. scale bar, north arrow, title, labels). `mapsf` maps `sf` objects
on `base` graphics.

A "Get Started" **vignette** contains commented scripts on how to create
various maps:
[`vignette(topic = "mapsf", package = "mapsf")`](https://riatelab.github.io/mapsf/articles/mapsf.md)

## Symbology

These functions display cartographic layers.

- [`mf_map()`](https://riatelab.github.io/mapsf/reference/mf_map.md)
  Plot a map

- [`mf_label()`](https://riatelab.github.io/mapsf/reference/mf_label.md)
  Plot labels

- [`mf_raster()`](https://riatelab.github.io/mapsf/reference/mf_raster.md)
  Plot a raster

- [`mf_graticule()`](https://riatelab.github.io/mapsf/reference/mf_graticule.md)
  Plot graticules

## Map layout

These functions are dedicated to the map layout design.

- [`mf_theme()`](https://riatelab.github.io/mapsf/reference/mf_theme.md)
  Set a theme

- [`mf_shadow()`](https://riatelab.github.io/mapsf/reference/mf_shadow.md)
  Plot a shadow

- [`mf_background()`](https://riatelab.github.io/mapsf/reference/mf_background.md)
  Plot a background image

- [`mf_annotation()`](https://riatelab.github.io/mapsf/reference/mf_annotation.md)
  Plot an annotation

- [`mf_arrow()`](https://riatelab.github.io/mapsf/reference/mf_arrow.md)
  Plot a north arrow

- [`mf_credits()`](https://riatelab.github.io/mapsf/reference/mf_credits.md)
  Plot credits

- [`mf_layout()`](https://riatelab.github.io/mapsf/reference/mf_layout.md)
  Plot a map layout

- [`mf_title()`](https://riatelab.github.io/mapsf/reference/mf_title.md)
  Plot a title

- [`mf_scale()`](https://riatelab.github.io/mapsf/reference/mf_scale.md)
  Plot a scale bar

- [`mf_inset_on()`](https://riatelab.github.io/mapsf/reference/mf_inset_on.md)
  /
  [`mf_inset_off()`](https://riatelab.github.io/mapsf/reference/mf_inset_on.md)
  Plot an inset

- [`mf_worldmap()`](https://riatelab.github.io/mapsf/reference/mf_worldmap.md)
  Plot a point on a world map

- [`mf_legend()`](https://riatelab.github.io/mapsf/reference/mf_legend.md)
  Plot a legend

## Utility functions

- [`mf_svg()`](https://riatelab.github.io/mapsf/reference/mf_svg.md)
  Export a map in SVG file format

- [`mf_png()`](https://riatelab.github.io/mapsf/reference/mf_png.md)
  Export a map in SVG file format

- [`mf_distr()`](https://riatelab.github.io/mapsf/reference/mf_distr.md)
  Plot a distribution

- [`mf_get_links()`](https://riatelab.github.io/mapsf/reference/mf_get_links.md)
  Get a link layer from a data.frame of links

- [`mf_get_pal()`](https://riatelab.github.io/mapsf/reference/mf_get_pal.md)
  Get color palettes

- [`mf_get_breaks()`](https://riatelab.github.io/mapsf/reference/mf_get_breaks.md)
  Get class intervals

- [`mf_get_mtq()`](https://riatelab.github.io/mapsf/reference/mf_get_mtq.md)
  Get the 'mtq' dataset

- [`mf_get_ratio()`](https://riatelab.github.io/mapsf/reference/mf_get_ratio.md)
  Get map width and height values

- [`mf_get_pencil()`](https://riatelab.github.io/mapsf/reference/mf_get_pencil.md)
  Get a pencil layer from polygons

- [`mf_get_borders()`](https://riatelab.github.io/mapsf/reference/mf_get_borders.md)
  Get a border layer from polygons

## See also

Useful links:

- <https://riatelab.github.io/mapsf/>

- Report bugs at <https://github.com/riatelab/mapsf/issues/>

## Author

**Maintainer**: Timothée Giraud <timothee.giraud@cnrs.fr>
([ORCID](https://orcid.org/0000-0002-1932-3323))

Other contributors:

- Hugues Pecout ([ORCID](https://orcid.org/0000-0002-0246-0954)) (Logo)
  \[contributor\]

- Ronan Ysebaert ([ORCID](https://orcid.org/0000-0002-7344-5911)) (Cheat
  sheet) \[contributor\]

- Elina Marveaux ([ORCID](https://orcid.org/0009-0000-8667-3019))
  (Themes) \[contributor\]

- Ian Fellows (No overlap algorithm for labels, from wordcloud package)
  \[copyright holder\]

- Jim Lemon (Arc drawing algorithm for annotations, from plotrix
  package) \[copyright holder\]

- Danielle Navarro ([ORCID](https://orcid.org/0000-0001-7648-6578))
  (Bézier curve algorithm for text annotations) \[copyright holder\]
