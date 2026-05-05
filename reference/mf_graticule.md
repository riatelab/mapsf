# Plot graticules

Display graticules and labels on a map.

## Usage

``` r
mf_graticule(
  x,
  col,
  lwd = 1,
  lty = 1,
  expandBB = rep(0, 4),
  label = TRUE,
  pos = c("top", "left"),
  cex = 0.7,
  extent = x,
  bg,
  add = TRUE
)
```

## Arguments

- x:

  object of class `sf`, `sfc` or `SpatRaster`

- col:

  graticules and label color

- lwd:

  graticules line width

- lty:

  graticules line type

- expandBB:

  fractional values to expand the bounding box with, in each direction
  (bottom, left, top, right)

- label:

  whether to add labels (TRUE) or not (FALSE)

- pos:

  labels positions ("bottom", "left", "top" and / or "right")

- cex:

  labels size

- extent:

  object with an `st_bbox` method to define plot extent; defaults to
  `x`. `extent` and `x` must use the same CRS.

- bg:

  background color of the map, hex code or color name given by
  [colors](https://rdrr.io/r/grDevices/colors.html), ignored if
  `add = TRUE`

- add:

  whether to add the layer to an existing plot (TRUE) or not (FALSE)

## Value

An (invisible) layer of graticules is returned (LINESTRING).

## Use of graticules

From
[`sf::st_graticule()`](https://r-spatial.github.io/sf/reference/st_graticule.html):
"In cartographic visualization, the use of graticules is not advised,
unless the graphical output will be used for measurement or navigation,
or the direction of North is important for the interpretation of the
content, or the content is intended to display distortions and artifacts
created by projection. Unnecessary use of graticules only adds visual
clutter but little relevant information. Use of coastlines,
administrative boundaries or place names permits most viewers of the
output to orient themselves better than a graticule."

## Examples

``` r
mtq <- mf_get_mtq()
mf_map(mtq, expandBB = c(0, .1, .1, 0))
mf_graticule(mtq)


mf_graticule(
  x = mtq,
  col = "coral4",
  lwd = 2,
  lty = 2,
  expandBB = c(.1, 0, 0, .1),
  label = TRUE,
  pos = c("right", "bottom"),
  cex = .8,
  add = FALSE
)
mf_map(mtq, add = TRUE)
```
