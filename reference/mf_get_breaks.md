# Get class intervals

A function to classify continuous variables.

This function is a wrapper for
[`classIntervals`](https://r-spatial.github.io/classInt/reference/classIntervals.html)
with some additional methods.

## Usage

``` r
mf_get_breaks(x, nbreaks, breaks, k = 1, central = FALSE, ...)
```

## Arguments

- x:

  a vector of numeric values. NA and Inf values are not used in the
  classification.

- nbreaks:

  a number of classes

- breaks:

  a classification method; the main methods are "quantile", "equal",
  "msd", "ckmeans" (natural breaks), "Q6" and "geom". See Details for
  the full list.

- k:

  number of standard deviation for "msd" method (see Details)

- central:

  creation of a central class for "msd" method (see Details)

- ...:

  further arguments of
  [`classIntervals`](https://r-spatial.github.io/classInt/reference/classIntervals.html)

## Value

A numeric vector of breaks

## Details

### `classInt` methods

"fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust",
"bclust", "fisher", "jenks", "dpih", "headtails", "maximum", and "box"
are
[`classIntervals`](https://r-spatial.github.io/classInt/reference/classIntervals.html)
methods. You may need to pass additional arguments for some of them.

### Natural breaks method

The "jenks", "fisher" and "ckmeans" methods are based on the same
concept of **natural breaks** and and produce similar groupings. The use
of "ckmeans" is recommended.

- The "jenks" method produces class boundaries falling on data points
  and is slow.

- The "fisher" method produces class boundaries located more
  conveniently between data points, and is faster than the "jenks"
  method.

- The "ckmeans" method produces exactly the same class boundaries as the
  "fisher" method, but is much faster. It uses the optimal univariate
  k-means method from the `Ckmeans.1d.dp` package. If the "ckmeans"
  method is selected but the `Ckmeans.1d.dp` package is not installed
  then the "fisher" method is used.

The relative speeds of these three methods may vary depending on the
number of data points and the number of classes.

### Other methods

The "msd" method is based on the **mean** and the **standard deviation**
of a numeric vector. The `nbreaks` parameter is not relevant, use `k`
and `central` instead. `k` indicates the extent of each class in share
of standard deviation. If `central=TRUE` then the mean value is the
center of a class else the mean is a break value.  
  
The "q6" method uses the following
[`quantile`](https://rdrr.io/r/stats/quantile.html) probabilities: 0,
0.05, 0.275, 0.5, 0.725, 0.95, 1.  
  
The "Q6" method uses the following
[`quantile`](https://rdrr.io/r/stats/quantile.html) probabilities: 0,
0.05, 0.25, 0.5, 0.75, 0.95, 1.  
  
The "geom" method is based on a geometric progression along the variable
values, all values must be strictly greater than zero.  
  
The "arith" method is based on an arithmetic progression along the
variable values.  
  
The "em" method is based on nested averages computation.

### Class boundaries

Breaks defined by a numeric vector or a classification method are
left-closed: breaks defined by `c(2, 5, 10, 15, 20)` will be mapped as
\[2 - 5\[, \[5 - 10\[, \[10 - 15\[, \[15 - 20\].

## See also

[classIntervals](https://r-spatial.github.io/classInt/reference/classIntervals.html)

## Examples

``` r
mtq <- mf_get_mtq()
mf_get_breaks(x = mtq$MED, nbreaks = 6, breaks = "quantile")
#> [1] 11929.0 13667.0 14786.0 15685.5 16860.0 18622.0 21761.0
```
