# Deprecated - Initialize a map with a specific extent

This function is deprecated. Please use
`mf_map(x, col = NA, border = NA)` instead.

Plot an invisible layer with the extent of a spatial object.  
Always use `add = TRUE` in
[`mf_map()`](https://riatelab.github.io/mapsf/reference/mf_map.md) calls
following an `mf_init()` call. This function is similar to
`mf_map(x, col = NA, border = NA)`.

## Usage

``` r
mf_init(x, expandBB = rep(0, 4), extent = x, bgc)
```

## Arguments

- x:

  object of class `sf`, `sfc` or `SpatRaster`

- expandBB:

  fractional values to expand the bounding box with, in each direction
  (bottom, left, top, right)

- extent:

  object with an st_bbox method to define plot extent; defaults to x.
  extent and x must use the same CRS.

## Value

No return value, a map is initiated.

## Examples

``` r
mtq <- mf_get_mtq()
target <- mtq[30, ]
mf_map(target, type = "base", col = NA, border = NA)
mf_map(mtq, add = TRUE)

# or
mf_map(mtq, extent = target)
```
