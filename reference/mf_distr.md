# Plot a distribution

This function displays a histogram, a box plot, a strip chart and a
density curve on the same plot.

## Usage

``` r
mf_distr(x, nbins, bw)
```

## Arguments

- x:

  a numeric variable

- nbins:

  number of bins in the histogram

- bw:

  bandwidth of the density curve

## Value

The number of bins of the histogram and the bandwidth of the density
curve are (invisibly) returned in a list.

## Examples

``` r
(mf_distr(rnorm(1000)))

#> $bw
#> [1] 0.2828805
#> 
#> $nbins
#> [1] 10
#> 
mf_distr(rbeta(1000, .6, 7))

mf_distr(rbeta(1000, 5, .6))
```
