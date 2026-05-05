# Plot labels

Put labels on a map.

## Usage

``` r
mf_label(
  x,
  var,
  col,
  cex = 0.7,
  overlap = TRUE,
  lines = TRUE,
  halo = FALSE,
  bg,
  r = 0.1,
  q = 1,
  ...
)
```

## Arguments

- x:

  object of class `sf`

- var:

  name of the variable to map

- col:

  labels color, it can be a single color or a vector of colors

- cex:

  labels cex, it can be a single size or a vector of sizes

- overlap:

  if FALSE, labels are moved so they do not overlap.

- lines:

  if TRUE, then lines are plotted between x,y and the word, for those
  words not covering their x,y coordinate

- halo:

  if TRUE, a 'halo' is displayed around the text and additional
  arguments bg and r can be modified to set the color and width of the
  halo.

- bg:

  halo color, it can be a single color or a vector of colors

- r:

  width of the halo, it can be a single value or a vector of values

- q:

  quality of the non overlapping labels placement. Possible values are 0
  (quick results), 1 (reasonable quality and speed), 2 (better quality),
  3 (insane quality, can take a lot of time).

- ...:

  further [text](https://rdrr.io/r/graphics/text.html) arguments.

## Value

No return value, labels are displayed.

## Examples

``` r
mtq <- mf_get_mtq()
mf_map(mtq)
mtq$cex <- c(rep(.8, 8), 2, rep(.8, 25))
mf_label(
  x = mtq, var = "LIBGEO",
  col = "grey10", halo = TRUE, cex = mtq$cex,
  overlap = FALSE, lines = FALSE
)
```
