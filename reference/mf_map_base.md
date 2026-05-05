# Plot a base map

[`mf_map()`](https://riatelab.github.io/mapsf/reference/mf_map.md) can
be used to display geographic layers (`sf` objects), using the default
map type **base**.

### Usage

For polygons:

    mf_map(x, col, border, lwd = 0.7, lty = 1,
           alpha, expandBB, extent, bg, add = FALSE)

For points:

    mf_map(x, col, border, pch = 20, cex = 1, lwd = 0.7,
           alpha, expandBB, extent, bg, add = FALSE)

For lines:

    mf_map(x, col, lwd = .7, lty = 1,
           alpha, expandBB, extent, bg, add = FALSE)

## Arguments

- x:

  object of class `sf`, `sfc` or `sfg`

- col:

  a color, hex code or color name given by
  [colors](https://rdrr.io/r/grDevices/colors.html). The default color
  for polygons is the foreground color, the default color for points and
  lines the highlight color (see
  [mf_theme](https://riatelab.github.io/mapsf/reference/mf_theme.md)).

- border:

  border color for polygons and points symbols, hex code or color name
  given by [colors](https://rdrr.io/r/grDevices/colors.html). The
  default color for polygon is the highlight color, the default color
  for points is the foreground color (see
  [mf_theme](https://riatelab.github.io/mapsf/reference/mf_theme.md)).

- lwd:

  border width for polygons and points symbols, lines width

- lty:

  type of line for polygons borders and lines

- pch:

  type of symbol to use for points, see
  [pch](https://rdrr.io/r/graphics/points.html)

- cex:

  symbols size, 2 means 2 times bigger

- alpha, expandBB, extent, bg, add:

  arguments described in
  [mf_map](https://riatelab.github.io/mapsf/reference/mf_map.md)

## Value

x is (invisibly) returned.

## See also

[`mf_map()`](https://riatelab.github.io/mapsf/reference/mf_map.md)

## Examples

``` r
mtq <- mf_get_mtq()
pts <- mf_get_mtq("points")
flows <- mf_get_mtq("lines")
mf_map(mtq, lty = 3)
mf_map(pts, col = "red", border = "white", pch = 21, add = TRUE)
mf_map(flows, col = "coral", lwd = 2, add = TRUE)
```
