

# Export a map in PNG format

[**Source code**](https://github.com/riatelab/mapsf//tree/master/R/mf_png.R#L31)

## Description

Export a map with the extent of a spatial object in PNG format.

PNG is a raster graphics file format and PNG export should be used for
maps that do not require further modification.

If <code>width</code> is specified, then <code>height</code> is deduced
from the width/height ratio of <code>x</code>. Alternatively, if
<code>height</code> is specified, then <code>width</code> is deduced
from the width/height ratio of <code>x</code>. This helps to produce
maps without too much wasted space.

Use <code>dev.off</code> to finish the export (see Examples).

## Usage

<pre><code class='language-R'>mf_png(x, filename = "map.png", width, height, expandBB = rep(0, 4), ...)
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
<code id="filename">filename</code>
</td>
<td>
path to the exported file
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="width">width</code>
</td>
<td>
width of the figure (pixels)
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="height">height</code>
</td>
<td>
height of the figure (pixels)
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
<code id="...">…</code>
</td>
<td>
further parameters
</td>
</tr>
</table>

## Value

No return value, a PNG device is initiated.

## Examples

``` r
library("mapsf")

mtq <- mf_get_mtq()
(filename <- tempfile(fileext = ".png"))
```

    [1] "/tmp/RtmpNCI9wU/fileeb5caf2ff5.png"

``` r
mf_png(mtq, filename = filename)
mf_map(mtq)
mf_title()
dev.off()
```

    png 
      2 
