

# Export a map in SVG format

[**Source code**](https://github.com/riatelab/mapsf//tree/master/R/mf_svg.R#L44)

## Description

Export a map with the extent of a spatial object in SVG format.

SVG export is the perfect solution for editing maps with desktop vector
graphics software. SVG is a vector graphics file format.

If <code>width</code> is specified, then <code>height</code> is deduced
from the width/height ratio of <code>x</code>. Alternatively, if
<code>height</code> is specified, then <code>width</code> is deduced
from the width/height ratio of <code>x</code>. This helps to produce
maps without too much wasted space.

Use <code>dev.off</code> to finish the export (see Examples).

## Usage

<pre><code class='language-R'>mf_svg(
  x,
  filename = "map.svg",
  width,
  height,
  expandBB = rep(0, 4),
  svglite = TRUE,
  ...
)
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
width of the figure (inches)
</td>
</tr>
<tr>
<td style="white-space: nowrap; font-family: monospace; vertical-align: top">
<code id="height">height</code>
</td>
<td>
height of the figure (inches)
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
<code id="svglite">svglite</code>
</td>
<td>
if TRUE, the export is done with the <code>svglite</code> package if it
is installed (see Details)
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

## Details

The default driver for building SVG files,
<code>grDevices::svg()</code>, has limitations regarding speed, file
size, editability, and font support. The <code>svglite</code> package
aims to solve these issues but it is not lightweight in terms of
dependencies, so it is not imported by <code>mapsf</code>, but rather
suggested.

However, we strongly recommend its use if the aim is to edit the maps
after export.

## Value

No return value, an SVG device is initiated.

## Examples

``` r
library("mapsf")

mtq <- mf_get_mtq()
(filename <- tempfile(fileext = ".svg"))
```

    [1] "/tmp/Rtmp4eZgE8/fileec915871f265.svg"

``` r
mf_svg(mtq, filename = filename)
mf_map(mtq)
mf_title()
dev.off()
```

    png 
      2 
