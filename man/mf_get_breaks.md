

# Get class intervals

[**Source code**](https://github.com/riatelab/mapsf//tree/master/R/mf_get_breaks.R#L64)

## Description

A function to classify continuous variables.

This function is a wrapper for <code>classIntervals</code> with some
additional methods.

## Usage

<pre><code class='language-R'>mf_get_breaks(x, nbreaks, breaks, k = 1, central = FALSE, ...)
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="x">x</code>
</td>
<td>
a vector of numeric values. NA and Inf values are not used in the
classification.
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="nbreaks">nbreaks</code>
</td>
<td>
a number of classes
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="breaks">breaks</code>
</td>
<td>
a classification method; one of "fixed", "sd", "equal", "pretty",
"quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih",
"q6", "Q6", geom", "arith", "em", "msd" or "ckmeans" (see Details)
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="k">k</code>
</td>
<td>
number of standard deviation for "msd" method (see Details)
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="central">central</code>
</td>
<td>
creation of a central class for "msd" method (see Details)
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="...">…</code>
</td>
<td>
further arguments of <code>classIntervals</code>
</td>
</tr>
</table>

## Details

"fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust",
"bclust", "fisher", "jenks" and "dpih" are <code>classIntervals</code>
methods. You may need to pass additional arguments for some of
them.<br /><br />

The "jenks", "fisher" and "ckmeans" methods are based on the same
concept of <strong>natural breaks</strong> and and produce similar
groupings.

<ul>
<li>

The "jenks" method produces class boundaries falling on data points and
is slow.

</li>
<li>

The "fisher" method produces class boundaries located more conveniently
between data points, and is faster than the "jenks" method.

</li>
<li>

The "ckmeans" method produces exactly the same class boundaries as the
"fisher" method, but is much faster. It uses the optimal univariate
k-means method from the <code>Ckmeans.1d.dp</code> package. If the
"ckmeans" method is selected but the <code>Ckmeans.1d.dp</code> package
is not installed then the "fisher" method is used.

</li>
</ul>

The relative speeds of these three methods may vary depending on the
number of data points and the number of classes.<br /><br />

The "q6" method uses the following <code>quantile</code> probabilities:
0, 0.05, 0.275, 0.5, 0.725, 0.95, 1.<br /><br /> The "Q6" method uses
the following <code>quantile</code> probabilities: 0, 0.05, 0.25, 0.5,
0.75, 0.95, 1.<br /><br /> The "geom" method is based on a geometric
progression along the variable values, all values must be strictly
greater than zero.<br /><br /> The "arith" method is based on an
arithmetic progression along the variable values.<br /><br /> The "em"
method is based on nested averages computation.<br /><br /> The "msd"
method is based on the mean and the standard deviation of a numeric
vector. The <code>nbreaks</code> parameter is not relevant, use
<code>k</code> and <code>central</code> instead. <code>k</code>
indicates the extent of each class in share of standard deviation. If
<code>central=TRUE</code> then the mean value is the center of a class
else the mean is a break value.

## Value

A numeric vector of breaks

## See Also

classIntervals

## Examples

``` r
library("mapsf")

mtq <- mf_get_mtq()
mf_get_breaks(x = mtq$MED, nbreaks = 6, breaks = "quantile")
```

    [1] 11929.0 13667.0 14786.0 15685.5 16860.0 18622.0 21761.0
