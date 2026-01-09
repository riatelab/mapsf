# Get the optimal position of a legend

Find the optimal for one or two legends. The optimal position is a
position that minimizes overlap between a spatial object and a legend.

## Usage

``` r
mf_get_leg_pos(x, n = 1)
```

## Arguments

- x:

  object of class `sf`

- n:

  number of positions to get (1 or 2)

## Value

A vector of position is returned

## Examples

``` r
mtq <- mf_get_mtq()
mf_get_leg_pos(mtq)
#> [1] "topright"
```
