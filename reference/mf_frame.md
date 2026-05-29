# Plot a frame

Plot a frame around an existing map.

## Usage

``` r
mf_frame(extent = "map", col, lwd = 1.5, lty = 1, ...)
```

## Arguments

- extent:

  type of frame, either 'map' or 'figure'

- col:

  line color, hex code or color name given by
  [colors](https://rdrr.io/r/grDevices/colors.html). The default color
  is the highlight color (see
  [mf_theme](https://riatelab.github.io/mapsf/reference/mf_theme.md)).

- lwd:

  line width

- lty:

  line type

- ...:

  other arguments from
  [`graphics::box()`](https://rdrr.io/r/graphics/box.html)

## Value

No return value, a frame is displayed.

## Examples

``` r
mtq <- mf_get_mtq()
mf_map(mtq)
mf_title()
mf_frame(extent = "map")

mf_map(mtq)
mf_title()
mf_frame(extent = "figure")
```
