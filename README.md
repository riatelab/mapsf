
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mapsf <img src="man/figures/logo.png" align="right" width="120"/>

<!-- badges: start -->

[![codecov](https://codecov.io/gh/rcarto/mapsf/branch/master/graph/badge.svg?token=G8MZTHC9KQ)](undefined)
[![R-CMD-check](https://github.com/riatelab/mapsf/workflows/R-CMD-check/badge.svg)](https://github.com/riatelab/mapsf/actions)
<!-- badges: end -->

Create and integrate thematic maps in your R workflow. This package
helps to design various cartographic representations such as
proportional symbols, choropleth or typology maps. It also offers
several functions to display layout elements that improve the graphic
presentation of maps (e.g. scale bar, north arrow, title, labels).
`mapsf` maps `sf` objects on `base` graphics.

## Installation

<!-- You can install the released version of mapsf  -->
<!-- from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("mapsf") -->
<!-- ``` -->
<!-- Alternatively,  -->

You can install the development version of `mapsf` from GitHub with:

``` r
remotes::install_github("riatelab/mapsf")
```

## Examples

This is a basic example which shows you how to create a map with
`mapsf`. The main `mapsf` function is `mf_map()`.

``` r
library(mapsf)
mtq <- mf_get_mtq()
mf_map(x = mtq)
mf_map(x = mtq, var = "POP", type = "prop")
mf_layout(frame = TRUE)
```

![](man/figures/README-unnamed-chunk-2-1.png)<!-- -->

A more detailed example:

``` r
mf_init(x = mtq, theme = "dark", shadow = TRUE, shadow_col = "grey10",
        export = "svg", filename = "man/figures/mtq.svg", width = 6, 
        expandBB = c(0,0,0,.3)) 
mf_map(x = mtq, var = "MED", type = "choro",
       pal = "Dark Mint", 
       breaks = "quantile", 
       nbreaks = 6, 
       leg_title = "Median Income\n(euros)", 
       leg_val_rnd = -2, 
       add = T)
mf_title("Wealth in Martinique, 2015")
mf_credits("T. Giraud\nSources: INSEE & IGN, 2018")
mf_scale(size = 5)
mf_arrow('topleft')
dev.off()
```

![](man/figures/mtq.svg)

## Main features

The **type** column indicates the value to use for the `type` argument
in `mf_map(x, var, type)`. The **data** column displays the relevant
data types for each map types.

![](man/figures/features.png)

## Background

This package aims at reproducing the core features of
[`cartography`](https://github.com/riatelab/cartography) with a more
coherent and modern API. It is lighter (less dependencies) and more
user-friendly.

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
