suppressPackageStartupMessages(library("terra"))

f <- rast(system.file("ex/elev.tif", package = "terra"))
b <- rast(system.file("ex/logo.tif", package = "terra"))
expect_silent(mf_raster(b))
expect_silent(mf_raster(f))
