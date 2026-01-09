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

  line color

- lwd:

  line width

- lty:

  line type

- ...:

  other arguments from [`box`](https://rdrr.io/r/graphics/box.html)

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
