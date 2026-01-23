# init
mtq <- mf_get_mtq()
expect_silent(mf_init(mtq))
expect_silent(mf_init(mtq, expandBB = c(0, 0, 0, .4)))
expect_silent(mf_init(mtq, bgc = "red"))
expect_silent(mf_init(mtq, extent = mtq[3, ]))
expect_silent(mf_init(mtq, extent = mtq[3, ], bgc = "red",
                      expandBB = c(0, 0, 0, .4)))
mf_theme(frame = "map")
expect_silent(mf_init(mtq))



b <- terra::rast(system.file("ex/elev.tif", package = "terra"))
expect_silent(mf_init(b))
mf_raster(b, add = TRUE)
expect_silent(mf_init(b, expandBB = c(0, 0, 0, .4)))
expect_silent(mf_init(b, bgc = "red"))
expect_silent(mf_init(b, extent = b))
expect_silent(mf_init(b, extent = b, bgc = "red",
                      expandBB = c(0, 0, 0, .4)))
