# Get the 'mtq' dataset

Import the mtq dataset (Martinique municipalities).

## Usage

``` r
mf_get_mtq(x = "polygons")
```

## Source

### For polygons (municipalities shapes) and points (municipalities centroids)

[Base comparateur de
territoires](https://www.insee.fr/fr/statistiques/2521169) (data, upload
date: 2018-09-25) & ADMIN EXPRESS-COG (geometry, 2018 edition).

Citation: Insee and IGN, 2018

### For lines (professional mobility flows from Fort-de-France to other municipalities)

[Flux de mobilité - déplacements domicile-lieu de
travail](https://www.insee.fr/fr/statistiques/3566477) (upload date:
2018-08-01)

Citation: Insee, 2018

## Arguments

- x:

  one of "polygons", "points", "lines"

## Value

an sf object

## Details

This a wrapper around
`st_read(system.file("gpkg/mtq.gpkg", package = "mapsf"), layer = x, quiet = TRUE)`.

### For polygons (municipalities shapes) and points (municipalities centroids)

- **INSEE_COM**: Municipality identifier

- **STATUS**: Municipality administrative status

- **LIBGEO**: Municipality name

- **POP**: Total population, 2015

- **MED**: Median disposable income adjusted per equivalent household
  member, in euros, 2015

- **CHOM**: Unemployed population, 2015

- **ACT**: Active population, 2015

### For lines (professional mobility flows from Fort-de-France to other municipalities)

- **i**: Municipality of residence identifier

- **j**: Municipality of workplace identifier

- **fij**: Flows of workers (employed population, 15 y.o. or more, 2015,
  only flows \\ 100)

- **sj**: Administrative status of the workplace municipality

## Examples

``` r
mtq <- mf_get_mtq()
```
