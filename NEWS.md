# mapsf 0.8.0

## Fix
- raise an error and display an informative message if no points are plotted 
when type = "prop*"

## Feat
- add mf_distr(), a function to display a statistical distribution with 
histogram, boxplot, stripchart and density curve
- add a lot of legend related args (leg_frame_border, leg_horiz, leg_adj, 
leg_bg, leg_fg, leg_size, leg_border, leg_box_border, leg_box_cex)
- double legends (prop_choro, prop_typo, etc.) are stacked by default
- add legends for raster
- add rev arg for function using pal to reverse named palettes

## Refactor
- use the maplegend pkg for legends (this change may introduce minor breaking 
changes)
- use s2 instead of overcomplicated sf code for othographic proj in 
mf_worldmap()




# mapsf 0.7.1

## Fix
- fix bug in color assignment in 'choro', 'prop_choro' and 'symb_choro' maps 
when supplying "incomplete" breaks (#56) 

# mapsf 0.7.0

## Fix
- use a vector of colors for lines if necessary in mf_label() (#50)
- enable pipe without side effects (no extra plot)
- use a default transparent background for insets 
- deprecate "theme" arg in relevant function, adapt docs and vignettes, use 
options() for themes instead of global variable & mimic the behaviour of 
basetheme package
- force the use of cairo device, if available, in mf_export() for png
- fix wrong class allocation when using breaks = "jenks" (#53)
- exports using unprojected objects do not produce figures with inaccurate 
height/width ratio anymore. 

## Feat
- add expandBB arg in mf_map()
- add expandBB arg in mf_raster()
- add arg checking depending on type in mf_map()
- add an error message for functions that need a pre-existing plot
- add mf_graticule() to add graticule lines and labels
- set internally the 'add' arg for each map types in mf_map()



# mapsf 0.6.1


## Fix
- add explicit support for sfg objects in mf_base(), mf_map(..., type = "base")


# mapsf 0.6.0


## Fix
- use val_max in mf_prop() for LINES
- add message when mf_export() is based on longlat obj
- add a test for "x" class in mf_map() 
- enable proportional symbol plots for single points  when using "prop", "prop_typo" & "prop_choro" maps; see #45
- fix a bug in color assignment when there is a mismatch between the number of classes and the size of the color palette
- add an error for method "geom" in mf_get_breaks() when min(x) <= 0
- remove Inf values from x when using mf_get_breaks()
- remove Inf values from x when using "prop", "prop_typo" & "prop_choro" maps
- add informative message when NA, 0 & Inf values are removed when using "prop", "prop_typo" & "prop_choro" maps
- add informative message when negative values are transformed to positive values when using "prop", "prop_typo" & "prop_choro" maps

## Feat
- add "prop_typo"" maps for LINES objects 
- add mf_get_ratio() to get appropriate values for map width & height






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
