# Get map width and height values

This function is to be used to get width and height values for maps
created in reports (\*.Rmd, \*.qmd).  
It uses the width / height ratio of a spatial object bounding box to
find a matching ratio for the map.  
If width is specified, then height is deduced from the width / height
ratio of x, figure margins and title size.  
If height is specified, then width is deduced from the width / height
ratio of x, figure margins and title size.

## Usage

``` r
mf_get_ratio(x, width, height, expandBB = rep(0, 4), theme = mf_theme())
```

## Arguments

- x:

  object of class `sf`, `sfc` or `SpatRaster`

- width:

  width of the figure (inches), use only one of width or height

- height:

  height of the figure (inches), use only one of width or height

- expandBB:

  expension of the map area in each direction (bottom, left, top,
  right). The expension is expressed as a share of `x` width (for left
  and right values) or a share of `x` height (for bottom and top
  values).

- theme:

  theme used for the map

## Value

Width and height are returned in inches.

## Examples

``` r
mtq <- mf_get_mtq()
mf_get_ratio(x = mtq, width = 5)
#> [1] 5.000 6.171
```
