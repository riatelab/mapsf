# Export a map in PNG format

Export a map with the extent of a spatial object in PNG format.

PNG is a raster graphics file format and PNG export should be used for
maps that do not require further modification.

If `width` is specified, then `height` is deduced from the width/height
ratio of `x`. Alternatively, if `height` is specified, then `width` is
deduced from the width/height ratio of `x`. This helps to produce maps
without too much wasted space.

Use [`dev.off()`](https://rdrr.io/r/grDevices/dev.html) to finish the
export (see Examples).

## Usage

``` r
mf_png(
  x,
  filename = "map.png",
  width,
  height,
  expandBB = rep(0, 4),
  res = 96,
  ...
)
```

## Arguments

- x:

  object of class `sf`, `sfc` or `SpatRaster`

- filename:

  path to the exported file

- width:

  width of the figure (pixels)

- height:

  height of the figure (pixels)

- expandBB:

  expension of the map area in each direction (bottom, left, top,
  right). The expension is expressed as a share of `x` width (for left
  and right values) or a share of `x` height (for bottom and top
  values).

- res:

  nominal resolution in ppi

- ...:

  further parameters

## Value

No return value, a PNG device is initiated.

## Examples

``` r
mtq <- mf_get_mtq()
(filename <- tempfile(fileext = ".png"))
#> [1] "/tmp/RtmpI8cWov/file17df627534462.png"
mf_png(mtq, filename = filename)
mf_map(mtq)
mf_title()
dev.off()
#> agg_record_17df64b3ef4a7 
#>                        2 
```
