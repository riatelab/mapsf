# Plot a base map

Plot an sf object. Here we specify the specific args and values applied
to the type. Args common to all map type are described in
[`mf_map()`](https://riatelab.github.io/mapsf/reference/mf_map.md).

### Usage

For polygons:

    mf_map(x, col = "grey80", border = "grey20", lwd = 0.7, add = FALSE, ...)

For points:

    mf_map(x, col = "grey80", pch = 20, cex = 1, border = "grey20",
           lwd = 0.7, add = FALSE, ...)

For lines:

    mf_map(x, col = "grey80", border = "grey20", lty = 1, lwd = 0.7,
           add = FALSE, ...)

## Arguments

- x:

  object of class `sf`, `sfc` or `sfg`

- col:

  a color, hex code or color name given by
  [`colors`](https://rdrr.io/r/grDevices/colors.html)

- border:

  border color

- lwd:

  border width

- pch:

  pch (point type) for symbols

- add:

  whether to add the layer to an existing plot (TRUE) or not (FALSE)

- cex:

  point size

- lty:

  line or border type

- ...:

  other parameters described in
  [`mf_map()`](https://riatelab.github.io/mapsf/reference/mf_map.md).
  leg\_\*, expandBB, extent, bg, alpha.

## Value

x is (invisibly) returned.

## See also

Other mf_map:
[`mf_map()`](https://riatelab.github.io/mapsf/reference/mf_map.md),
[`mf_map-prop`](https://riatelab.github.io/mapsf/reference/mf_map-prop.md)

## Examples

``` r
library(sf)
mtq <- mf_get_mtq()
mf_map(mtq, type = "base")

mf_map(mtq, type = "base", col = "blue")
```
