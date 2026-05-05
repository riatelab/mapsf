# Plot a logo on a map

The logo can be a PNG or JPG/JPEG file.

## Usage

``` r
mf_logo(filename, pos = "bottomright", cex = 1, adj = c(0, 0), resize = TRUE)
```

## Arguments

- filename:

  filename of the logo image, PNG or JPG/JPEG format.

- pos:

  position of the logo, one of 'topleft', 'top','topright', 'right',
  'bottomright', 'bottom', 'bottomleft', 'left' or a vector of two
  coordinates in map units (c(x, y)). Use 'interactive' to choose the
  legend position by clicking on the map.

- cex:

  amount by which the logo width should be magnified or reduced relative
  to the default

- adj:

  adjust the position of the logo in x and y directions

- resize:

  if FALSE, the logo is displayed at its original size in pixels and
  `cex` is not used.

## Value

No return value, a logo is displayed.

## Examples

``` r
m <- mf_get_mtq()
mf_map(m)
mf_scale()
logo <- system.file("img", "Rlogo.png", package = "png")
mf_logo(logo, pos = "bottomleft", adj = c(0, 4))
mf_credits()
```
