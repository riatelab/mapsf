# Plot a point on a world map

Plot a point on a world map.

## Usage

``` r
mf_worldmap(
  x,
  lon,
  lat,
  water_col = "lightblue",
  land_col = "grey60",
  border_col = "grey40",
  border_lwd = 0.8,
  ...
)
```

## Arguments

- x:

  object of class `sf` or `sfc`

- lon:

  longitude

- lat:

  latitude

- water_col:

  color of the water

- land_col:

  color of the land

- border_col:

  color of the borders

- border_lwd:

  width of the borders

- ...:

  further parameters related to the plotted point aspect (cex, pch,
  col...)

## Value

No return value, a world map is displayed.

## Note

The main part of the code is stolen from @fzenoni
(<https://gist.github.com/fzenoni/ef23faf6d1ada5e4a91c9ef23b0ba2c1>).

## Examples

``` r
mtq <- mf_get_mtq()
mf_worldmap(mtq)

mf_worldmap(lon = 24, lat = 39)

mf_worldmap(
  lon = 106, lat = 26,
  pch = 4, lwd = 3, cex = 2, col = "tomato4",
  water_col = "#232525", land_col = "#A9B7C6",
  border_col = "white", border_lwd = 1
)
```
