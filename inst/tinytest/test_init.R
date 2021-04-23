# init
mtq <- mf_get_mtq()
expect_silent(mf_init(mtq, theme = "darkula"))
b <- raster::brick(system.file("external/rlogo.grd", package="raster"))
expect_silent(mf_init(b))
