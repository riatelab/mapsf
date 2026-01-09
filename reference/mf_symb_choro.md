# Plot symbols using choropleth coloration

Plot symbols with colors based on a quantitative data classification.

## Usage

``` r
mf_symb_choro(
  x,
  var,
  pal,
  alpha = NULL,
  rev = FALSE,
  breaks = "quantile",
  nbreaks,
  border,
  pch,
  cex = 2,
  lwd = 0.7,
  pch_na = 4,
  cex_na = 1,
  col_na = "white",
  val_order,
  leg_pos = mf_get_leg_pos(x, 1),
  leg_title = var,
  leg_title_cex = c(0.8, 0.8),
  leg_val_cex = c(0.6, 0.6),
  leg_val_rnd = 2,
  leg_val_dec = ".",
  leg_val_big = "",
  leg_no_data = c("No data", "No data"),
  leg_frame = c(FALSE, FALSE),
  leg_frame_border,
  leg_horiz = FALSE,
  leg_adj = c(0, 0),
  leg_fg,
  leg_bg,
  leg_size = 1,
  leg_box_border,
  leg_box_cex = c(1, 1),
  add = TRUE
)
```

## Arguments

- x:

  object of class `sf`

- var:

  name(s) of the variable(s) to plot

- pal:

  a set of colors or a palette name (from
  [hcl.colors](https://rdrr.io/r/grDevices/palettes.html))

- alpha:

  opacity, in the range \[0,1\]

- rev:

  if `pal` is a [hcl.colors](https://rdrr.io/r/grDevices/palettes.html)
  palette name, whether the ordering of the colors should be reversed
  (TRUE) or not (FALSE)

- breaks:

  either a numeric vector with the actual breaks, or a classification
  method name (see
  [mf_get_breaks](https://riatelab.github.io/mapsf/reference/mf_get_breaks.md)
  and Details)

- nbreaks:

  number of classes

- border:

  border color

- pch:

  pch (point type) for symbols

- cex:

  cex (point size) for symbols

- lwd:

  border width

- pch_na:

  point type for NA values

- cex_na:

  point size for NA values

- col_na:

  color for missing values

- val_order:

  values order, a character vector that matches var modalities

- leg_pos:

  position of the legend, two of 'topleft', 'top','topright',
  'right','bottomright', 'bottom', 'bottomleft', 'left' or a vector of
  two coordinates in map units (c(x, y)). leg_pos argument can be
  c('position', 'position'), c('position', x2, y2), c(x1,y1, 'position')
  or c(x1, y1, x2, y2). Use NA to avoid plotting the legend, use
  'interactive' to choose thelegend position interactively.

- leg_title:

  legend title

- leg_title_cex:

  size of the legend title

- leg_val_cex:

  size of the values in the legend

- leg_val_rnd:

  number of decimal places of the values in the legend

- leg_val_dec:

  decimal separator

- leg_val_big:

  thousands separator

- leg_no_data:

  label for missing values

- leg_frame:

  whether to add a frame to the legend (TRUE) or not (FALSE)

- leg_frame_border:

  border color of the legend frame

- leg_horiz:

  display the legend horizontally (for proportional symbols and
  choropleth types)

- leg_adj:

  adjust the postion of the legend in x and y directions

- leg_fg:

  color of the legend foreground

- leg_bg:

  color of the legend backgournd

- leg_size:

  size of the legend; 2 means two times bigger

- leg_box_border:

  border color of legend boxes

- leg_box_cex:

  width and height size expansion of boxes

- add:

  whether to add the layer to an existing plot (TRUE) or not (FALSE)

## Value

x is (invisibly) returned.

## Details

Breaks defined by a numeric vector or a classification method are
left-closed: breaks defined by `c(2, 5, 10, 15, 20)` will be mapped as
\[2 - 5\[, \[5 - 10\[, \[10 - 15\[, \[15 - 20\]. The "jenks" method is
an exception and has to be right-closed. Jenks breaks computed as
`c(2, 5, 10, 15, 20)` will be mapped as \[2 - 5\], \]5 - 10\], \]10 -
15\], \]15 - 20\].

## Examples

``` r
mtq <- mf_get_mtq()
mf_map(mtq)
mf_map(mtq, c("STATUS", "MED"), "symb_choro")


mf_map(mtq)
mtq$STATUS[30] <- NA
mtq$MED[5] <- NA
mf_map(mtq, c("STATUS", "MED"),
  type = "symb_choro",
  pal = "Reds 3", breaks = "quantile", nbreaks = 4,
  pch = 21:23, cex = c(3, 2, 1),
  pch_na = 25, cex_na = 1.5, col_na = "blue",
  val_order = c(
    "Prefecture",
    "Sub-prefecture",
    "Simple municipality"
  )
)
```
