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

  shadow color, hex code or color name given by
  [colors](https://rdrr.io/r/grDevices/colors.html). The default color
  is the highlight color (see
  [mf_theme](https://riatelab.github.io/mapsf/reference/mf_theme.md)).

- cex:

  shadow extent

- add:

  whether to add the layer to an existing plot (TRUE) or not (FALSE)

- extent:

  `sf` object used to define the map extent; defaults to `x`. `extent`
  and `x` must use the same CRS.

- bg:

  background color of the map, hex code or color name given by
  [colors](https://rdrr.io/r/grDevices/colors.html), ignored if
  `add = TRUE`

- expandBB:

  expension of the map area in each direction (bottom, left, top,
  right). The expension is expressed as a share of `x` width (for left
  and right values) or a share of `x` height (for bottom and top
  values).

## Value

x is (invisibly) returned.

## Examples

``` r
mtq <- mf_get_mtq()
mf_shadow(mtq)
mf_map(mtq, add = TRUE)
```
