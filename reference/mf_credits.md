# Plot credits

Plot credits (sources, author, year...).

## Usage

``` r
mf_credits(
  txt = "Source(s) & Author(s)",
  pos = "bottomleft",
  col,
  cex = 0.6,
  font = 3,
  bg = NA
)
```

## Arguments

- txt:

  text of the credits, use '\n' to add line breaks

- pos:

  position, one of 'bottomleft', 'bottomright' or 'rightbottom'

- col:

  color of the text, hex code or color name given by
  [colors](https://rdrr.io/r/grDevices/colors.html). The default color
  is the highlight color (see
  [mf_theme](https://riatelab.github.io/mapsf/reference/mf_theme.md)).

- cex:

  size of the text

- font:

  font of the text

- bg:

  background color, hex code or color name given by
  [colors](https://rdrr.io/r/grDevices/colors.html)

## Value

No return value, credits are displayed.

## Examples

``` r
mtq <- mf_get_mtq()
mf_map(mtq)
mf_credits(txt = "Author\nSources - Year")
```
