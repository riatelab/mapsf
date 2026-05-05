# Plot proportional symbols with choropleth coloration

[`mf_map()`](https://riatelab.github.io/mapsf/reference/mf_map.md) with
**prop_choro** type creates symbols that are proportional to values of a
first variable and colored to reflect the classification of a second
variable.

This map types uses two variables and some arguments need to be set for
both variables (see Details).

For polygons, centroids are used to plot proportional symbols. This map
type is not available for lines.

### Usage

For polygons and points:

    mf_map(x, var, type = "prop_choro",
           inches = 0.3, val_max,  symbol = "circle",
           pal, rev = FALSE, breaks = "quantile", nbreaks,
           border, lwd = 0.7, col_na = "white",
           alpha, expandBB, extent, bg, add = TRUE, leg_*)

## Arguments

- x:

  object of class `sf` (polygons or points)

- var:

  names of the variables to map. The first value refers to the
  proportional symbols, the second one to the choropleth coloration.

- type:

  "prop_choro"

- inches:

  size of the largest symbol in inches (radius for circles, half width
  for squares)

- val_max:

  maximum value corresponding to the largest symbol or line

- symbol:

  type of proportional symbols, either "circle" or "square"

- breaks:

  either a numeric vector with the actual breaks, or a classification
  method name. The main methods are 'quantile', 'equal', 'msd',
  'ckmeans' (natural breaks), 'Q6' and 'geom'. See
  [mf_get_breaks](https://riatelab.github.io/mapsf/reference/mf_get_breaks.md)
  for details.

- nbreaks:

  number of classes

- pal:

  a set of colors (hex codes) or a palette name. Palette names can be
  obtained with [hcl.pals](https://rdrr.io/r/grDevices/palettes.html).
  The default palette is the pal_seq palette (see
  [mf_theme](https://riatelab.github.io/mapsf/reference/mf_theme.md)).

- rev:

  if `pal` is a palette name, whether the ordering of the colors should
  be reversed (TRUE) or not (FALSE)

- border:

  border color of proportional symbols, a hex code or color name given
  by [colors](https://rdrr.io/r/grDevices/colors.html). The default
  color is the background color (see
  [mf_theme](https://riatelab.github.io/mapsf/reference/mf_theme.md)).

- lwd:

  border width of proportional symbols

- col_na:

  color for missing values, a hex code or a color name given by
  [colors](https://rdrr.io/r/grDevices/colors.html)

- alpha, expandBB, extent, bg, add:

  arguments described in
  [mf_map](https://riatelab.github.io/mapsf/reference/mf_map.md)

- leg\_\*:

  legend arguments described in
  [mf_map](https://riatelab.github.io/mapsf/reference/mf_map.md). See
  details for arguments with two values.

## Value

x is (invisibly) returned.

## Details

Legend arguments that need two values are: 'leg_title', 'leg_val_rnd',
and 'leg_horiz'. The first values refers to the proportional symbols
legend, the second one to the choropleth legend.

## See also

[`mf_map()`](https://riatelab.github.io/mapsf/reference/mf_map.md),
[mf_map_prop](https://riatelab.github.io/mapsf/reference/mf_map_prop.md),
[mf_map_choro](https://riatelab.github.io/mapsf/reference/mf_map_choro.md),
[`mf_distr()`](https://riatelab.github.io/mapsf/reference/mf_distr.md),
[`mf_get_breaks()`](https://riatelab.github.io/mapsf/reference/mf_get_breaks.md),
[`mf_get_pal()`](https://riatelab.github.io/mapsf/reference/mf_get_pal.md)

## Examples

``` r
mtq <- mf_get_mtq()
mf_map(mtq)
mtq[6, "MED"] <- NA
mf_map(
  x = mtq, var = c("POP", "MED"), type = "prop_choro",
  inches = .2,
  val_max = 90000, symbol = "circle",
  col_na = "grey90", pal = "Cividis",
  breaks = "msd", nbreaks = 4, lwd = 1,
  leg_pos = "topright",
  leg_title = c("Population", "Median Income"),
  leg_val_rnd = c(0, 1),
  leg_horiz = c(TRUE, FALSE),
  leg_title_cex = .9,
  leg_val_dec = ",",
  leg_val_cex = .8,
  leg_size = 1,
  add = TRUE
)
```
