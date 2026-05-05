# Deprecated - Export a map

This function is deprecated. Please use
[`mf_png`](https://riatelab.github.io/mapsf/reference/mf_png.md) or
[`mf_svg`](https://riatelab.github.io/mapsf/reference/mf_svg.md)
instead.

Export a map with the extent of a spatial object.  
The map is exported in PNG or SVG format.  
If only one of `width` or `height` is set, `mf_export` uses the
width/height ratio of `x` bounding box to find a matching ratio for the
export.  
Always use `add = TRUE` in `mf_map` calls following an `mf_export`
call.  
Use `dev.off` to finish the export (see Examples).

## Usage

``` r
mf_export(
  x,
  filename = "map.png",
  width,
  height,
  res = 96,
  ...,
  expandBB = rep(0, 4)
)
```

## Arguments

- x:

  object of class `sf`, `sfc` or `SpatRaster`

- filename:

  path to the exported file. If the file extension is ".png" a png
  graphic device is opened, if the file extension is ".svg" a svg
  graphic device is opened.

- width:

  width of the figure (pixels for png, inches for svg)

- height:

  height of the figure (pixels for png, inches for svg)

- res:

  resolution (for png)

- ...:

  further parameters for png or svg export

- expandBB:

  fractional values to expand the bounding box with, in each direction
  (bottom, left, top, right)

## Value

No return value, a map file is initiated (in PNG or SVG format).

## Examples

``` r
mtq <- mf_get_mtq()
(filename <- tempfile(fileext = ".png"))
#> [1] "/tmp/Rtmpy7mVeK/file7c1351b400eba.png"
mf_png(mtq, filename = filename)
mf_map(mtq)
mf_title()
dev.off()
#> agg_record_7c135443cb7de 
#>                        2 
```
