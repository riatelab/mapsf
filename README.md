
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mapsf <img src="man/figures/logo.png" align="right" width="120"/>

<!-- badges: start -->

[![CRAN](https://www.r-pkg.org/badges/version/mapsf)](https://cran.r-project.org/package=mapsf)
[![R-CMD-check](https://github.com/riatelab/mapsf/workflows/R-CMD-check/badge.svg)](https://github.com/riatelab/mapsf/actions)
[![codecov](https://codecov.io/gh/riatelab/mapsf/branch/master/graph/badge.svg?token=TPK6HZOLWH)](https://codecov.io/gh/riatelab/mapsf)
[![status](https://tinyverse.netlify.com/badge/mapsf)](https://CRAN.R-project.org/package=mapsf)
<!-- badges: end -->

Create and integrate thematic maps in your R workflow. This package
helps to design various cartographic representations such as
proportional symbols, choropleth or typology maps. It also offers
several functions to display layout elements that improve the graphic
presentation of maps (e.g. scale bar, north arrow, title, labels).
`mapsf` maps `sf` objects on `base` graphics.

## Installation

You can install the released version of mapsf from
[CRAN](https://cran.r-project.org/package=mapsf) with:

``` r
install.packages("mapsf")
```

Alternatively, you can install the development version of `mapsf` from
GitHub with:

``` r
remotes::install_github("riatelab/mapsf")
```

## Usage

This is a basic example which shows how to create a map with `mapsf`.  
The main `mapsf` function is `mf_map()`.

``` r
library(mapsf)
#> Loading required package: sf
#> Linking to GEOS 3.7.1, GDAL 3.1.2, PROJ 7.1.0
# Import the sample dataset
mtq <- mf_get_mtq()
# Plot the base map
mf_map(x = mtq)
# Plot proportional symbols
mf_map(x = mtq, var = "POP", type = "prop")
# Plot a map layout
mf_layout(title = "Population in Martinique", 
          credits = "T. Giraud; Sources: INSEE & IGN, 2018", 
          frame = TRUE)
box(which = "figure")
```

![](man/figures/README-example1-1.png)<!-- -->

A more detailed example:

``` r
# Initiate a map figure with a theme and extra margins 
mf_init(x = mtq, theme = "dark", expandBB = c(0,0,0,.3),
        export = "png", filename = "mtq.png", width = 600) 
# Plot a shadow
mf_shadow(mtq, col = "grey10", add = TRUE)
# Plot a choropleth map
mf_map(x = mtq, var = "MED", type = "choro",
       pal = "Dark Mint", 
       breaks = "quantile", 
       nbreaks = 6, 
       leg_title = "Median Income\n(euros)", 
       leg_val_rnd = -2, 
       add = TRUE)
# Start an inset map
mf_inset_on(x = "worldmap", pos = "right")
# Plot the position of the sample dataset on a worlmap
mf_worldmap(mtq, col = "#0E3F5C")
# Close the inset
mf_inset_off()
# Plot a title
mf_title("Wealth in Martinique, 2015")
# Plot credits
mf_credits("T. Giraud\nSources: INSEE & IGN, 2018")
# Plot a scale bar
mf_scale(size = 5)
# Plot a north arrow
mf_arrow('topleft')
dev.off()
```

![](man/figures/mtq.png)

Note that `mapsf` is, to a certain degree, compatible with the pipe
syntax:

``` r
library(magrittr)
mf_theme("agolalight")
mtq %>% 
  mf_map() %>%
  mf_map(c("POP","STATUS"), "prop_typo")
mf_title()
```

![](man/figures/README-example4-1.png)<!-- -->

## Main features

The **type** column indicates the value to use for the `type` argument
in `mf_map(x, var, type)`. The **data** column displays the relevant
data types for each map types.

![](man/figures/features.png)

## Background

`mapsf` is the successor of
[`cartography`](https://github.com/riatelab/cartography). There are no
plans for new features or enhancements in `cartography`, but basic
maintenance and support will continue indefinitely. Existing projects
that use `cartography` can safely continue to use `cartography`. New
projects should use `mapsf` because it is friendlier, lighter and more
robust.  
See [`mapsf`
vignette](https://riatelab.github.io/mapsf/articles/mapsf.html#symbology)
to migrate from `cartography` to `mapsf`.

## Alternatives

-   [cartography](https://github.com/riatelab/cartography)
-   [tmap](https://github.com/mtennekes/tmap)  
-   [ggplot2](https://github.com/tidyverse/ggplot2) +
    [ggspatial](https://github.com/paleolimbot/ggspatial)  
-   [oceanis](https://github.com/insee-psar-at/oceanis-package)

## Community Guidelines

One can contribute to the package through [pull
requests](https://github.com/riatelab/mapsf/pulls) and report issues or
ask questions [here](https://github.com/riatelab/mapsf/issues).  
This project uses [conventional
commits](https://www.conventionalcommits.org/en/v1.0.0-beta.3/) and
[semantic versioning](https://semver.org/).
