# Get a border layer from polygons

This function extracts borders between contiguous polygons.

## Usage

``` r
mf_get_borders(x)
```

## Arguments

- x:

  an sf object of POLYGONS, using a projected CRS

## Value

An sf object (MULTILINESTRING) of borders is returned.

## Note

If the polygon layer contains topology errors (such as contiguous
polygons not sharing exactly the same boundary) the function may not
return all boundaries correctly. It is possible to use
[`st_snap()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html)
or other functions to try and correct these errors.

## Examples

``` r
mtq <- mf_get_mtq()
mtq_b <- mf_get_borders(mtq)
mf_map(mtq)
mf_map(mtq_b, col = 1:5, lwd = 4, add = TRUE)
```
