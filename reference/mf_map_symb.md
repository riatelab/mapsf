# Plot symbols

[`mf_map()`](https://riatelab.github.io/mapsf/reference/mf_map.md) can
use symbols to display qualitative data, using **symb** map type.

For polygons, centroids are used to plot graduated symbols. This map
type is not available for lines.

### Usage

For polygons and points:

    mf_map(x, var, type = "symb",
           pch, cex = 2, lwd = 0.7, pal, rev = FALSE, border,
           val_order,
           col_na = "grey", pch_na = 4, cex_na = 1,
           alpha, expandBB, extent, bg, add = TRUE, leg_*)

## Arguments

- x:

  object of class `sf` (polygons or points)

- var:

  name of the variable to map

- type:

  "symb"

- pch:

  a vector of types of symbols, see
  [pch](https://rdrr.io/r/graphics/points.html). The length of `pch`
  should match the number of modalities.

- cex:

  a vector of sizes for symbols. The length of `cex` should match the
  number of modalities.

- lwd:

  border width of symbols

- pal:

  a set of colors (hex codes) or a palette name. Palette names can be
  obtained with [hcl.pals](https://rdrr.io/r/grDevices/palettes.html).
  The default palette is the pal_quali palette (see
  [mf_theme](https://riatelab.github.io/mapsf/reference/mf_theme.md)).

- rev:

  if `pal` is a palette name, whether the ordering of the colors should
  be reversed (TRUE) or not (FALSE)

- border:

  border color for symbols, a hex code or color name given by
  [colors](https://rdrr.io/r/grDevices/colors.html). The default color
  is the background color (see
  [mf_theme](https://riatelab.github.io/mapsf/reference/mf_theme.md)).

- val_order:

  modalities order in the legend, a character vector that matches `var`
  modalities. Default to alphabetic order of modalities.

- pch_na:

  type of symbol for missing values, see
  [pch](https://rdrr.io/r/graphics/points.html)

- cex_na:

  size of symbol for missing values

- col_na:

  color for missing values, a hex code or a color name given by
  [colors](https://rdrr.io/r/grDevices/colors.html)

## Value

x is (invisibly) returned.

## Examples

``` r
mtq <- mf_get_mtq()
mtq$STATUS[3] <- NA
mf_map(mtq)
mf_map(mtq, "STATUS", "symb",
  pal = "Berlin", border = "white", lwd = 1,
  cex = c(4, 3, 2), pch = c(21:23), col_na = "red",
  val_order = c("Prefecture", "Sub-prefecture", "Simple municipality"),
  leg_title = ""
)
```
