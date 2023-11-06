suppressPackageStartupMessages(library("terra"))

f <- rast(system.file("ex/elev.tif", package = "terra"))
b <- rast(system.file("ex/logo.tif", package = "terra"))
expect_silent(mf_raster(b))
expect_silent(mf_raster(f))
expect_silent(mf_raster(f, pal = "Reds"))
expect_silent(mf_raster(f, pal = c("red", "#646464")))
expect_error(mf_raster("not a raster"))
expect_error(mf_raster(f, pal = "not a palette name"))

