# Export a map in PNG format

Export a map with the extent of a spatial object in PNG format.

PNG is a raster graphics file format and PNG export should be used for
maps that do not require further modification.

If `width` is specified, then `height` is deduced from the width/height
ratio of `x`. Alternatively, if `height` is specified, then `width` is
deduced from the width/height ratio of `x`. This helps to produce maps
without too much wasted space.

Use `dev.off` to finish the export (see Examples).

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

  fractional values to expand the bounding box with, in each direction
  (bottom, left, top, right)

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
#> [1] "/tmp/RtmpViEc0z/file423075ed0aa7b.png"
mf_png(mtq, filename = filename)
mf_map(mtq)
mf_title()
dev.off()
#> agg_record_417357845 
#>                    2 
```
