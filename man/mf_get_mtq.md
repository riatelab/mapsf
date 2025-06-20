

# Get the ‘mtq’ dataset

[**Source code**](https://github.com/riatelab/mapsf//tree/master/R/mf_get_mtq.R#L11)

## Description

Import the mtq dataset (Martinique municipalities).

## Usage

<pre><code class='language-R'>mf_get_mtq()
</code></pre>

## Details

This a wrapper around <code>st_read(system.file(“gpkg/mtq.gpkg”, package
= “mapsf”),quiet = TRUE)</code>.

## Value

an sf object of Martinique municipalities

## Examples

``` r
library("mapsf")

mtq <- mf_get_mtq()
```
