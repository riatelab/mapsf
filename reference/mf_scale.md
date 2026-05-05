# Plot a scale bar

Plot a scale bar.

## Usage

``` r
mf_scale(
  size,
  pos = "bottomright",
  lwd = 1.5,
  cex = 0.6,
  col,
  crs_units = "m",
  scale_units = "km",
  adj = c(0, 0),
  x
)
```

## Arguments

- size:

  size of the scale bar in scale units (`scale_units`, default to km).
  If size is not set, an automatic size is used.

- pos:

  position. It can be one of 'bottomright', 'bottomleft', 'interactive'
  or a vector of two coordinates in map units (c(x, y)).

- lwd:

  line width of the scale bar

- cex:

  size of the scale bar text

- col:

  color of the scale bar (line and text)

- crs_units:

  units used in the CRS of the currently plotted layer. Possible values
  are "m" and "ft" (see Details).

- scale_units:

  units used for the scale bar. Can be "mi" for miles, "ft" for feet,
  "m" for meters, or "km" for kilometers (default).

- adj:

  adjust the position of the scale bar in x and y directions

- x:

  object of class crs, sf or sfc. If set, the CRS of x will be used
  instead of `crs_units` to define CRS units.

## Value

No return value, a scale bar is displayed.

## Details

Most CRS use the meter as unit. Some US CRS use feet or US survey feet.
If unsure of the unit used in the CRS you can use the x argument of the
function. Alternatively, you can use
`sf::st_crs(zz, parameters = TRUE)$units_gdal` to see which units are
used in the `zz` layer.

The scale bar cannot be displayed on unprojected (long/lat) maps or on
maps without documented CRS.

## Examples

``` r
mtq <- mf_get_mtq()
mf_map(mtq)
mf_scale()


library(sf)
nc <- st_read(system.file("shape/nc.shp", package = "sf"))[1, ]
#> Reading layer `nc' from data source `/home/tim/Documents/R/4.6/sf/shape/nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.32385 ymin: 33.88199 xmax: -75.45698 ymax: 36.58965
#> Geodetic CRS:  NAD27

nc_foot <- st_transform(nc, 2264) # NC state plane, US foot
mf_map(nc_foot)
mf_scale(size = 5, crs_units = "ft", scale_units = "mi")

mf_map(nc_foot)
mf_scale(size = 5, x = nc_foot, scale_units = "mi")


nc_meter <- st_transform(nc, 32119) # NC state plane, m
mf_map(nc_meter)
mf_scale(size = 5, crs_units = "m", scale_units = "mi")
mf_scale(size = 5, crs_units = "m", scale_units = "km", pos = "bottomleft")
```
