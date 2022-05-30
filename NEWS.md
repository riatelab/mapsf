# mapsf 0.5.0

## Fix
- remove "export" arg for exports based on terra rasters
- remove "bg"" arg in mf_map()
- add "pch = 20" default to plot points with mf_base()
- get sf back from Depends to Imports
- remove s2 related message for recent version of sf in mf_worldmap()

## Feat 
- add "interactive" position for legends, north arrow, scale bar, annotation
- add self-adjusted rounded values for proportional circles legends

# mapsf 0.4.0

## Fix
- allow the display of raster with >=2 bands (not only exactly 3)
- avoid mf_map(..., type="symb") failing when there is only one modality
- make mf_export() aware of the export format with the filename extension only (+ deprecate "export"" arg)
- add a default maximum to maxcell arg for raster display in mf_raster()
- change smooth defaults in mf_raster(), TRUE if nlyr>=2, FALSE otherwise 

## Feat
- add a web only vignette on faceted maps
- add a web only vignette on custom fonts
- add a cheat sheet
- add parameters to customize worldmaps (land and ocean colors and borders)
- add informatives messages concerning mf_map() input (checking type and variable names)


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
