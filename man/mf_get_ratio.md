

# Get map width and height values

[**Source code**](https://github.com/riatelab/mapsf//tree/master/R/mf_get_ratio.R#L23)

## Description

This function is to be used to get width and height values for maps
created in reports (*.Rmd, *.qmd).<br /> It uses the width / height
ratio of a spatial object bounding box to find a matching ratio for the
map.<br /> If width is specified, then height is deduced from the width
/ height ratio of x, figure margins and title size.<br /> If height is
specified, then width is deduced from the width / height ratio of x,
figure margins and title size.

## Usage

<pre><code class='language-R'>mf_get_ratio(x, width, height, expandBB = rep(0, 4), theme = mf_theme())
</code></pre>

## Arguments

<table role="presentation">
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="x">x</code>
</td>
<td>
object of class <code>sf</code>, <code>sfc</code> or
<code>SpatRaster</code>
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="width">width</code>
</td>
<td>
width of the figure (inches), use only one of width or height
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="height">height</code>
</td>
<td>
height of the figure (inches), use only one of width or height
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="expandBB">expandBB</code>
</td>
<td>
fractional values to expand the bounding box with, in each direction
(bottom, left, top, right)
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="theme">theme</code>
</td>
<td>
theme used for the map
</td>
</tr>
</table>

## Value

Width and height are returned in inches.

## Examples

``` r
library("mapsf")

mtq <- mf_get_mtq()
mf_get_ratio(x = mtq, width = 5)
```

    [1] 5.000 6.171
