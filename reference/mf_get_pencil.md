# Get a pencil layer from polygons

Create a pencil layer. This function transforms a POLYGON or
MULTIPOLYGON sf object into a MULTILINESTRING one.

## Usage

``` r
mf_get_pencil(x, size = 100, buffer = 0, lefthanded = TRUE, clip = FALSE)
```

## Arguments

- x:

  an sf object, a simple feature collection (POLYGON or MULTIPOLYGON).

- size:

  density of the penciling. Median number of points used to build the
  MULTILINESTRING.

- buffer:

  buffer around each polygon. This buffer (in map units) is used to take
  sample points. A negative value adds a margin between the penciling
  and the original polygons borders

- lefthanded:

  if TRUE the penciling is done left-handed style.

- clip:

  if TRUE, the penciling is cut by the original polygon.

## Value

A MULTILINESTRING sf object is returned.

## Examples

``` r
mtq <- mf_get_mtq()
mtq_pencil <- mf_get_pencil(x = mtq, clip = FALSE)
mf_map(mtq)
mf_map(mtq_pencil, add = TRUE)
```
