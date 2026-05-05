# Plot a map

`mf_map()` is the main function of the package, it displays map layers
on a georeferenced plot.

`mf_map()` has three main arguments:

- `x`, an sf object,

- `var`, the name(s) of a variable(s) to map,

- `type`, the map type.

Relevant arguments and default values are different for each map type
and are described in dedicated help pages (see
[base](https://riatelab.github.io/mapsf/reference/mf_map_base.md),
[choro](https://riatelab.github.io/mapsf/reference/mf_map_choro.md),
[typo](https://riatelab.github.io/mapsf/reference/mf_map_typo.md),
[prop](https://riatelab.github.io/mapsf/reference/mf_map_prop.md),
[prop_choro](https://riatelab.github.io/mapsf/reference/mf_map_prop_choro.md),
[prop_typo](https://riatelab.github.io/mapsf/reference/mf_map_prop_typo.md),
[symb](https://riatelab.github.io/mapsf/reference/mf_map_symb.md),
[grad](https://riatelab.github.io/mapsf/reference/mf_map_grad.md) or
[symb_choro](https://riatelab.github.io/mapsf/reference/mf_map_symb_choro.md)).

## Usage

``` r
mf_map(x, var, type = "base",
       breaks, nbreaks, pal, alpha, rev, inches, val_max, symbol, col,
       lwd_max, val_order, pch, cex, border, lwd, col_na, cex_na, pch_na,
       expandBB, extent, bg, add,
       leg_pos, leg_title, leg_title_cex, leg_val_cex, leg_val_rnd,
       leg_val_dec, leg_val_big, leg_no_data, leg_frame, leg_frame_border,
       leg_horiz, leg_adj, leg_bg, leg_fg, leg_size,
       leg_box_border, leg_box_cex, ...)
```

## Arguments

- x:

  object of class `sf`

- var:

  name(s) of the variable(s) to map

- type:

  - **[base](https://riatelab.github.io/mapsf/reference/mf_map_base.md)**:
    base maps

  - **[choro](https://riatelab.github.io/mapsf/reference/mf_map_choro.md)**:
    choropleth maps

  - **[typo](https://riatelab.github.io/mapsf/reference/mf_map_typo.md)**:
    typology maps

  - **[prop](https://riatelab.github.io/mapsf/reference/mf_map_prop.md)**:
    proportional symbols maps

  - **[prop_choro](https://riatelab.github.io/mapsf/reference/mf_map_prop_choro.md)**:
    proportional symbols with choropleth coloration

  - **[prop_typo](https://riatelab.github.io/mapsf/reference/mf_map_prop_typo.md)**:
    proportional symbols with typology coloration

  - **[symb](https://riatelab.github.io/mapsf/reference/mf_map_symb.md)**:
    symbols maps

  - **[grad](https://riatelab.github.io/mapsf/reference/mf_map_grad.md)**:
    graduated symbols maps

  - **[symb_choro](https://riatelab.github.io/mapsf/reference/mf_map_symb_choro.md)**:
    symbols with choropleth coloration

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

- alpha:

  `col` or `pal` opacity, in the range \[0,1\] (0 means transparent and
  1 means opaque). Default is set to 1.

- rev:

  if `pal` is a palette name, whether the ordering of the colors should
  be reversed (TRUE) or not (FALSE)

- inches:

  size of the largest symbol in inches (radius for circles, half width
  for squares)

- val_max:

  maximum value corresponding to the largest symbol or line

- symbol:

  type of proportional symbols, either "circle" or "square"

- col:

  a color, hex code or color name given by
  [colors](https://rdrr.io/r/grDevices/colors.html)

- lwd_max:

  width of the largest line

- val_order:

  modalities order in the legend, a character vector that matches `var`
  modalities

- pch:

  type of symbol to use for points, see
  [pch](https://rdrr.io/r/graphics/points.html)

- cex:

  symbols size, 2 means 2 times bigger

- border:

  border color for polygons or symbols. It can be a hex code or a color
  name given by [colors](https://rdrr.io/r/grDevices/colors.html).

- lwd:

  border width of polygons, symbols or lines

- col_na:

  color for missing values, a hex code or a color name given by
  [colors](https://rdrr.io/r/grDevices/colors.html).

- cex_na:

  symbols size for missing values on points

- pch_na:

  symbol to use for missing values on points, see
  [pch](https://rdrr.io/r/graphics/points.html)

- expandBB:

  fractional values to expand the bounding box with, in each direction
  (bottom, left, top, right)

- extent:

  object with an `st_bbox` method to define plot extent; defaults to
  `x`. `extent` and `x` must use the same CRS.

- bg:

  background color of the map, hex code or color name given by
  [colors](https://rdrr.io/r/grDevices/colors.html), ignored if
  `add = TRUE`

- add:

  whether to add the layer to an existing plot (TRUE) or not (FALSE)

- leg_pos:

  position of the legend, one of 'topleft', 'top','topright', 'right',
  'bottomright', 'bottom', 'bottomleft', 'left' or a vector of two
  coordinates in map units (c(x, y)). Use `NA` to avoid plotting the
  legend, use 'interactive' to choose the legend position by clicking on
  the map.

- leg_title:

  legend title

- leg_title_cex:

  size of the title

- leg_val_cex:

  size of the values

- leg_val_rnd:

  number of decimal places of the values displayed in the legend

- leg_val_dec:

  decimal separator

- leg_val_big:

  thousands separator

- leg_no_data:

  label for missing values

- leg_frame:

  whether to add a frame to the legend (TRUE) or not (FALSE)

- leg_frame_border:

  border color of the legend frame

- leg_horiz:

  display the legend horizontally (for proportional symbols and
  choropleth types)

- leg_adj:

  adjust the position of the legend in x and y directions

- leg_bg:

  color of the legend background

- leg_fg:

  color of the legend foreground

- leg_size:

  size of the legend. Combine this argument with `leg_title_cex` and
  `leg_val_cex`.

- leg_box_border:

  border color of legend boxes (for types related to choropleth and
  typology)

- leg_box_cex:

  width and height size expansion of boxes

- ...:

  ignored

## Value

x is (invisibly) returned.

## Examples

``` r
mtq <- mf_get_mtq()
# basic examples
# type = "base"
mf_map(mtq)
# type = "prop"
mf_map(mtq)
mf_map(mtq, var = "POP", type = "prop")

# type = "choro"
mf_map(mtq, var = "MED", type = "choro")

# type = "typo"
mf_map(mtq, "STATUS", "typo")

# type = "symb"
mf_map(mtq)
mf_map(mtq, "STATUS", "symb")

# type = "grad"
mf_map(mtq)
mf_map(mtq, var = "POP", type = "grad")

# type = "prop_choro"
mf_map(mtq)
mf_map(mtq, var = c("POP", "MED"), type = "prop_choro")

# type = "prop_typo"
mf_map(mtq)
mf_map(mtq, var = c("POP", "STATUS"), type = "prop_typo")

# type = "symb_choro
mf_map(mtq)
mf_map(mtq, var = c("STATUS", "MED"), type = "symb_choro")
```
