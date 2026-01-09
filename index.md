# mapsf

[![CRAN](https://www.r-pkg.org/badges/version-ago/mapsf)](https://cran.r-project.org/package=mapsf)
[![R-CMD-check](https://github.com/riatelab/mapsf/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/riatelab/mapsf/actions/workflows/R-CMD-check.yaml)
[![downloads](https://cranlogs.r-pkg.org/badges/mapsf?color=brightgreen)](https://cran.r-project.org/package=mapsf)
[![Codecov test
coverage](https://codecov.io/gh/riatelab/mapsf/branch/master/graph/badge.svg)](https://app.codecov.io/gh/riatelab/mapsf?branch=master)
[![status](https://tinyverse.netlify.app/badge/mapsf)](https://CRAN.R-project.org/package=mapsf)

**Create thematic maps and integrate them into your R workflow.**

This package helps you design different cartographic representations
such as proportional symbols, choropleths, or typology maps. It also
provides several functions for displaying layout elements that enhance
the graphical presentation of maps (e.g., scale bar, north arrow, title,
labels). `mapsf` maps `sf` objects on `base` graphics.

## Installation

You can install the released version of `mapsf` from
[CRAN](https://cran.r-project.org/package=mapsf) with:

``` r
install.packages("mapsf")
```

Alternatively, you can install the development version of `mapsf` (the
**dev** branch) from [r-universe](https://riatelab.r-universe.dev/mapsf)
with:

``` r
install.packages("mapsf", repos = "https://riatelab.r-universe.dev")
```

## Usage

This is a basic example which shows how to create a map with `mapsf`.

``` r
library(mapsf)
# Import the sample dataset
mtq <- mf_get_mtq()
# Plot the base map
mf_map(x = mtq)
# Plot proportional symbols
mf_map(x = mtq, var = "POP", type = "prop", leg_pos = "topright")
# Plot a map layout
mf_layout(
  title = "Population in Martinique",
  credits = "T. Giraud; Sources: INSEE & IGN, 2018"
)
```

![](reference/figures/example1-1.png)

You can find more detailed examples on the [“Get
Started”](https://riatelab.github.io/mapsf/articles/mapsf) section of
the [package website](https://riatelab.github.io/mapsf/) and you can
also download its [cheat
sheet](https://raw.githubusercontent.com/riatelab/mapsf/master/vignettes/fig/mapsf_cheatsheet.pdf).  
[![Cheat
Sheet](reference/figures/mapsf_cheatsheet.png)](https://raw.githubusercontent.com/riatelab/mapsf/master/vignettes/fig/mapsf_cheatsheet.pdf)

## Alternatives

- [tmap](https://github.com/r-tmap/tmap)  
- [ggplot2](https://github.com/tidyverse/ggplot2) +
  [ggspatial](https://github.com/paleolimbot/ggspatial)

## Community Guidelines

One can contribute to the package through [pull
requests](https://github.com/riatelab/mapsf/pulls) and report issues or
ask questions [here](https://github.com/riatelab/mapsf/issues). See the
[CONTRIBUTING.md](https://github.com/riatelab/mapsf/blob/master/CONTRIBUTING.md)
file for detailed instructions on how to contribute.  
This project uses [conventional
commits](https://www.conventionalcommits.org/en/v1.0.0-beta.3/) and
[semantic versioning](https://semver.org/).
