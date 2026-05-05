# Plot a shadow

Plot the shadow of a polygon layer.

## Usage

``` r
mf_shadow(
  x,
  col,
  cex = 1,
  add = FALSE,
  extent = x,
  bg,
  expandBB = rep(0.04, 4)
)
```

## Arguments

- x:

  an sf or sfc polygon object

- col:

  shadow color. The default color is the highlight color (see
  [mf_theme](https://riatelab.github.io/mapsf/reference/mf_theme.md)).

- cex:

  shadow extent

- add:

  whether to add the layer to an existing plot (TRUE) or not (FALSE)

- extent:

  object with an `st_bbox` method to define plot extent; defaults to
  `x`. `extent` and `x` must use the same CRS.

- bg:

  background color of the map, hex code or color name given by
  [colors](https://rdrr.io/r/grDevices/colors.html), ignored if
  `add = TRUE`

- expandBB:

  fractional values to expand the bounding box with, in each direction
  (bottom, left, top, right)

## Value

x is (invisibly) returned.

## Examples

``` r
mtq <- mf_get_mtq()
mf_shadow(mtq)
mf_map(mtq, add = TRUE)
```
