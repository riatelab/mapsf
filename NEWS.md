# mapsf 0.3.0

## Fix
* add support for native pipe
* suppress messages that appear when s2 use is switched with mf_worldmap()
* replace raster by terra in all things raster

## Feat
* add a web only vignette on insets
* add a web only vignette on themes
* add mf_background(), a function to plot a background image for map
* add support for raster in mf_export() and mf_init()


# mapsf 0.2.0

## Fix
* change default value for interpolate and display without extra margins in mf_raster()
* increase minimal R version to 3.6.0 to use hcl.colors()
* adjust the largest symbol size in mf_map(..., "prop*")
* remove frame around insets
* remove LazyData from DESCRIPTION
* use sf 'on the fly' projection for unprojected sf objects
* better display of titles and maps (tiny extra space around maps)
* add explicit default value for "add" arg in mapping functions
* allow to plot (coherent) GEOMETRYCOLLECTIONS
* better default for POINT and LINES in mf_map(..., type = "base")
* fix mf_worldmap() by bypassing s2 use


## Feat
* Added a `NEWS.md` file to track changes to the package.
* split mf_init() to mf_init() and mf_export()
* allow width AND height set in mf_export()
* allow raster as input in mf_init() and mf_export() 
* allow to change existent theme settings directrly in mf_theme()
* make mf_theme() return the current theme
* add default value for txt, add bg arg (for background) in mf_credits()
* add coordinates positioning for maps with 2 legends
* add alpha arg for transparency in mapping functions using "pal"
* change the default theme value to a theme with only tiny margins
* a web only vignette on map export
* allow hcl.colors palette names use in mf_legend*() functions
