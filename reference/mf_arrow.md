# Plot a north arrow

Plot a north arrow.

## Usage

``` r
mf_arrow(pos = "topleft", col, cex = 1, adj = c(0, 0), align)
```

## Arguments

- pos:

  position. It can be one of 'topleft', 'top','topright', 'right',
  'bottomright', 'bottom','bottomleft', 'left', 'interactive' or a
  vector of two coordinates in map units (c(x, y))

- col:

  arrow color, hex code or color name given by
  [colors](https://rdrr.io/r/grDevices/colors.html). The default color
  is the highlight color (see
  [mf_theme](https://riatelab.github.io/mapsf/reference/mf_theme.md)).

- cex:

  arrow size

- adj:

  adjust the position of the north arrow in x and y directions

- align:

  object of class `sf` or `sfc` used to adjust the arrow to the real
  north

## Value

No return value, a north arrow is displayed.

## Examples

``` r
mtq <- mf_get_mtq()
mf_map(mtq)
mf_arrow(pos = "topright")
```
