library("raster")

f <- raster(system.file("external/test.grd", package="raster"))
b <- brick(system.file("external/rlogo.grd", package="raster"))
expect_silent(mf_raster(b))
expect_silent(mf_raster(f))
