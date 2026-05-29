# Plot a title

Default values for `mf_title()` arguments are set with the map theme
(see
[mf_theme](https://riatelab.github.io/mapsf/reference/mf_theme.md)).

## Usage

``` r
mf_title(txt = "Map Title", pos, tab, bg, fg, cex, line, font, inner, banner)
```

## Arguments

- txt:

  title text

- pos:

  position, one of 'left', 'center', 'right'

- tab:

  if TRUE the title is displayed as a tab

- bg:

  background color of the title, hex code or color name given by
  [colors](https://rdrr.io/r/grDevices/colors.html)

- fg:

  foreground color of the title, hex code or color name given by
  [colors](https://rdrr.io/r/grDevices/colors.html)

- cex:

  size of the title

- line:

  number of margin lines used for the title

- font:

  font of the title

- inner:

  if TRUE the title is displayed inside the plot area; if FALSE the
  title is displayed in the top margin

- banner:

  if TRUE the title is displayed as a banner

## Value

No return value, a title is displayed.

## Examples

``` r
mtq <- mf_get_mtq()
mf_map(mtq)
mf_title()
```
