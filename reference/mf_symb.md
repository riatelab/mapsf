# Deprecated - Plot symbols

This function is deprecated. Please use
[`mf_map()`](https://riatelab.github.io/mapsf/reference/mf_map.md) with
`type = "symb"` instead.

Plot symbols based on qualitative data.

## Usage

``` r
mf_symb(
  x,
  var,
  pal,
  alpha = NULL,
  rev = FALSE,
  border,
  pch,
  cex = 2,
  lwd = 0.7,
  col_na = "grey",
  pch_na = 4,
  cex_na = 1,
  val_order,
  extent = x,
  bg,
  expandBB = rep(0.04, 4),
  leg_pos = mf_get_leg_pos(x),
  leg_title = var,
  leg_title_cex = 0.8,
  leg_val_cex = 0.6,
  leg_no_data = "No data",
  leg_frame = FALSE,
  leg_frame_border,
  leg_adj = c(0, 0),
  leg_fg,
  leg_bg,
  leg_size = 1,
  add = TRUE
)
```

## Arguments

- x:

  object of class `sf`

- var:

  name(s) of the variable(s) to plot

- pal:

  a set of colors (hex codes) or a palette name (valid palette names can
  be obtained with
  [`hcl.pals`](https://rdrr.io/r/grDevices/palettes.html)).

- alpha:

  opacity, in the range 0,1

- rev:

  if `pal` is a valid palette name, whether the ordering of the colors
  should be reversed (TRUE) or not (FALSE)

- border:

  border color

- pch:

  pch (point type) for symbols

- cex:

  cex (point size) for symbols

- lwd:

  border width

- col_na:

  color for missing values

- pch_na:

  point type for NA values

- cex_na:

  point size for NA values

- val_order:

  values order, a character vector that matches var modalities

- leg_pos:

  position of the legend, one of 'topleft', 'top','topright', 'right',
  'bottomright', 'bottom', 'bottomleft', 'left' or a vector of two
  coordinates in map units (c(x, y)). Use `NA` to avoid plotting the
  legend, use 'interactive' to choose the legend position by clicking on
  the map.

- leg_title:

  legend title

- leg_title_cex:

  size of the legend title

- leg_val_cex:

  size of the values in the legend

- leg_no_data:

  label for missing values

- leg_frame:

  whether to add a frame to the legend (TRUE) or not (FALSE)

- leg_frame_border:

  border color of the legend frame

- leg_adj:

  adjust the position of the legend in x and y directions

- leg_fg:

  color of the legend foreground

- leg_bg:

  color of the legend background

- leg_size:

  size of the legend; 2 means two times bigger

- add:

  whether to add the layer to an existing plot (TRUE) or not (FALSE)

## Value

x is (invisibly) returned.

## Examples

``` r
mtq <- mf_get_mtq()
mf_map(mtq)
mf_map(mtq, "STATUS", "symb")


mtq[6, "STATUS"] <- NA
mf_map(mtq)
mf_map(
  x = mtq, var = "STATUS", type = "symb",
  pch = c(21:23), pal = c("red1", "tan1", "khaki1"),
  border = "grey20", cex = c(1.5, 1, .9), lwd = .5,
  val_order = c("Prefecture", "Sub-prefecture", "Simple municipality"),
  pch_na = 24, leg_frame = TRUE
)
```
