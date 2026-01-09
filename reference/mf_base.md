# Plot an sf object

Plot an sf object. This is mostly a wrapper around
`plot(st_geometry(x), ...)`.

## Usage

``` r
mf_base(
  x,
  col,
  border,
  alpha = NULL,
  cex = 1,
  pch = 20,
  lwd = 0.7,
  lty = 1,
  add = FALSE,
  ...
)
```

## Arguments

- x:

  object of class `sf`, `sfc` or `sfg`

- col:

  color

- border:

  border color

- alpha:

  opacity, in the range \[0,1\]

- cex:

  point size

- pch:

  pch (point type) for symbols

- lwd:

  border width

- lty:

  line or border type

- add:

  whether to add the layer to an existing plot (TRUE) or not (FALSE)

- ...:

  ignored

## Value

x is (invisibly) returned.

## Examples

``` r
library(sf)
#> Linking to GEOS 3.13.1, GDAL 3.10.3, PROJ 9.6.0; sf_use_s2() is TRUE
mtq <- mf_get_mtq()
mf_map(mtq, type = "base")

mf_map(mtq, type = "base", col = "blue")
```
