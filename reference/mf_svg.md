# Export a map in SVG format

Export a map with the extent of a spatial object in SVG format.

SVG export is the perfect solution for editing maps with desktop vector
graphics software. SVG is a vector graphics file format.

If `width` is specified, then `height` is deduced from the width/height
ratio of `x`. Alternatively, if `height` is specified, then `width` is
deduced from the width/height ratio of `x`. This helps to produce maps
without too much wasted space.

Use \`dev.off() to finish the export (see Examples).

## Usage

``` r
mf_svg(
  x,
  filename = "map.svg",
  width,
  height,
  expandBB = rep(0, 4),
  svglite = TRUE,
  ...
)
```

## Arguments

- x:

  object of class `sf`, `sfc` or `SpatRaster`

- filename:

  path to the exported file

- width:

  width of the figure (inches)

- height:

  height of the figure (inches)

- expandBB:

  expension of the map area in each direction (bottom, left, top,
  right). The expension is expressed as a share of `x` width (for left
  and right values) or a share of `x` height (for bottom and top
  values).

- svglite:

  if TRUE, the export is done with the `svglite` package if it is
  installed (see Details)

- ...:

  further parameters

## Value

No return value, an SVG device is initiated.

## Details

The default driver for building SVG files,
[`grDevices::svg()`](https://rdrr.io/r/grDevices/cairo.html), has
limitations regarding speed, file size, editability, and font support.
The `svglite` package aims to solve these issues but it is not
lightweight in terms of dependencies, so it is not imported by `mapsf`,
but rather suggested.

However, we strongly recommend its use if the aim is to edit the maps
after export.

## Examples

``` r
mtq <- mf_get_mtq()
(filename <- tempfile(fileext = ".svg"))
#> [1] "/tmp/RtmpI8cWov/file17df676900873.svg"
mf_svg(mtq, filename = filename)
mf_map(mtq)
mf_title()
dev.off()
#> agg_record_17df66494466 
#>                       2 
```
