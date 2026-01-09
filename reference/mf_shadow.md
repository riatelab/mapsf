# Plot a shadow

Plot the shadow of a polygon layer.

## Usage

``` r
mf_shadow(x, col, cex = 1, add = FALSE)
```

## Arguments

- x:

  an sf or sfc polygon object

- col:

  shadow color

- cex:

  shadow extent

- add:

  whether to add the layer to an existing plot (TRUE) or not (FALSE)

## Value

x is (invisibly) returned.

## Examples

``` r
mtq <- mf_get_mtq()
mf_shadow(mtq)
mf_map(mtq, add = TRUE)
```
