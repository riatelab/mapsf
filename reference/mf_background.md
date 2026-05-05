# Plot a background image

Plot a background image on an existing plot

## Usage

``` r
mf_background(filename, ...)
```

## Arguments

- filename:

  filename of the background image, PNG or JPG/JPEG format.

- ...:

  ignored

## Value

No return value, a background image is displayed.

## Examples

``` r
if (require("jpeg")) {
  mtq <- mf_get_mtq()
  mf_map(mtq, col = NA, border = NA)
  mf_background(system.file("img/background.jpg", package = "mapsf"))
  mf_map(mtq, lwd = 3, col = NA, border = "white", add = TRUE)
  mf_credits(
    txt = "Background photo by Noita Digital on Unsplash",
    col = "white"
  )
}
#> Loading required package: jpeg
```
