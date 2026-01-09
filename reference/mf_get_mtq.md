# Get the 'mtq' dataset

Import the mtq dataset (Martinique municipalities).

## Usage

``` r
mf_get_mtq()
```

## Value

an sf object of Martinique municipalities

## Details

This a wrapper around
`st_read(system.file("gpkg/mtq.gpkg", package = "mapsf"),quiet = TRUE)`.

## Examples

``` r
mtq <- mf_get_mtq()
```
