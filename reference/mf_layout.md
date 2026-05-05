# Plot a map layout

Plot a map layout (title, credits, scalebar, north arrow, frame).

This function uses
[mf_title](https://riatelab.github.io/mapsf/reference/mf_title.md),
[mf_credits](https://riatelab.github.io/mapsf/reference/mf_credits.md),
[mf_scale](https://riatelab.github.io/mapsf/reference/mf_scale.md) and
[mf_arrow](https://riatelab.github.io/mapsf/reference/mf_arrow.md) with
default values.

## Usage

``` r
mf_layout(
  title = "Map Title",
  credits = "Authors & Sources",
  scale = TRUE,
  arrow = TRUE,
  frame = FALSE
)
```

## Arguments

- title:

  title of the map

- credits:

  credits

- scale:

  display a scale bar

- arrow:

  display an arrow

- frame:

  display a frame

## Value

No return value, a map layout is displayed.

## Examples

``` r
mtq <- mf_get_mtq()
mf_map(mtq)
mf_layout()
```
