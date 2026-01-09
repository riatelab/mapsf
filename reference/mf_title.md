# Plot a title

Plot a title

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

  background of the title

- fg:

  foreground of the title

- cex:

  cex of the title

- line:

  number of lines used for the title

- font:

  font of the title

- inner:

  if TRUE the title is displayed inside the plot area

- banner:

  if TRUE the title is dispalayed as a banner

## Value

No return value, a title is displayed.

## Examples

``` r
mtq <- mf_get_mtq()
mf_map(mtq)
mf_title()
```
