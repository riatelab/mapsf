# Plot symbols with choropleth coloration

[`mf_map()`](https://riatelab.github.io/mapsf/reference/mf_map.md) with
**symb_choro** type creates symbols that reflect modalities of a first
qualitative variable and colored to reflect the classification of a
second variable.

This map types uses two variables and some arguments need to be set for
both variables (see Details).

For polygons, centroids are used to plot symbols. This map type is not
available for lines.

### Usage

For polygons and points:

    mf_map(x, var, type = "symb_choro",
           pch, cex = 2, lwd = 0.7, border, val_order,
           pal, rev = FALSE, breaks = "quantile", nbreaks,
           pch_na = 4, cex_na = 1, col_na = "white",
           alpha, expandBB, extent, bg, add = TRUE, leg_*)

## Arguments

- x:

  object of class `sf` (polygons or points)

- var:

  names of the variables to map. The first value refers to the symbols
  categories, the second one to the choropleth coloration.

- type:

  "symb_choro"

- pch:

  a vector of types of symbols, see
  [pch](https://rdrr.io/r/graphics/points.html). The length of `pch`
  should match the number of modalities.

- cex:

  a vector of sizes for symbols. The length of `cex` should match the
  number of modalities.

- lwd:

  border width of symbols

- border:

  border color for symbols, a hex code or color name given by
  [colors](https://rdrr.io/r/grDevices/colors.html). The default color
  is the background color (see
  [mf_theme](https://riatelab.github.io/mapsf/reference/mf_theme.md)).

- val_order:

  modalities order in the legend, a character vector that matches `var`
  modalities. Default to alphabetic order of modalities.

- pal:

  a set of colors (hex codes) or a palette name. Palette names can be
  obtained with [hcl.pals](https://rdrr.io/r/grDevices/palettes.html).
  The default palette is the pal_seq palette (see
  [mf_theme](https://riatelab.github.io/mapsf/reference/mf_theme.md)).

- rev:

  if `pal` is a palette name, whether the ordering of the colors should
  be reversed (TRUE) or not (FALSE)

- breaks:

  either a numeric vector with the actual breaks, or a classification
  method name. The main methods are 'quantile', 'equal', 'msd',
  'ckmeans' (natural breaks), 'Q6' and 'geom'. See
  [mf_get_breaks](https://riatelab.github.io/mapsf/reference/mf_get_breaks.md)
  for details.

- nbreaks:

  number of classes

- pch_na:

  type of symbol for missing values, see
  [pch](https://rdrr.io/r/graphics/points.html)

- cex_na:

  size of symbol for missing values

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

Legend arguments that need two values are: 'leg_title', 'leg_no_data'.
The first value refers to the symbols legend, the second one to the
choropleth legend.

## See also

[`mf_map()`](https://riatelab.github.io/mapsf/reference/mf_map.md),
[mf_map_symb](https://riatelab.github.io/mapsf/reference/mf_map_symb.md),
[mf_map_choro](https://riatelab.github.io/mapsf/reference/mf_map_choro.md),
[`mf_distr()`](https://riatelab.github.io/mapsf/reference/mf_distr.md),
[`mf_get_breaks()`](https://riatelab.github.io/mapsf/reference/mf_get_breaks.md),
[`mf_get_pal()`](https://riatelab.github.io/mapsf/reference/mf_get_pal.md)

## Examples

``` r
mtq <- mf_get_mtq()
mf_map(mtq)
mtq$STATUS[4] <- NA
mf_map(mtq, c("STATUS", "MED"),
  type = "symb_choro", lwd = 1,
  pal = "Reds 3", breaks = "quantile", nbreaks = 4,
  cex = c(2, 1, 1), pch = c(20, 21, 23), pch_na = 22,
  leg_pos = "topright", border = "white",
  val_order = c("Prefecture", "Sub-prefecture", "Simple municipality")
)
```
