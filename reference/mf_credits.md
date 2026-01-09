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

  color

- cex:

  cex of the credits

- font:

  font of the credits

- bg:

  background color

## Value

No return value, credits are displayed.

## Examples

``` r
mtq <- mf_get_mtq()
mf_map(mtq)
mf_credits(txt = "Author\nSources - Year")
```
