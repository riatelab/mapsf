# Initialize a map with a specific extent

Plot an invisible layer with the extent of a spatial object.  
Always use `add = TRUE` in `mf_map` calls following an `mf_init` call.
This function is similar to `mf_map(x, col = NA, border = NA)`.

## Usage

``` r
mf_init(x, expandBB = rep(0, 4))
```

## Arguments

- x:

  object of class `sf`, `sfc` or `SpatRaster`

- expandBB:

  fractional values to expand the bounding box with, in each direction
  (bottom, left, top, right)

## Value

No return value, a map is initiated.

## Examples

``` r
mtq <- mf_get_mtq()
target <- mtq[30, ]
mf_init(target)
mf_map(mtq, add = TRUE)
```
