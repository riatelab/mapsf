# init
mtq <- mf_get_mtq()
expect_silent(mf_init(mtq))
expect_silent(mf_init(mtq, expandBB = c(0,0,0,.4)))
expect_warning(mf_init(mtq, theme = "darkula"))
b <- terra::rast(system.file("ex/elev.tif", package="terra"))
expect_silent(mf_init(b))
mf_raster(b, add = T)
