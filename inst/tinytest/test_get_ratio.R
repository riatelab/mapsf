expect_silent(mapsf:::x_to_bb(terra::rast(nrows=2, ncols=2, xmin=0, xmax=4),
                              expandBB = rep(0, 4)))
expect_silent(mapsf:::x_to_bb(mtq, expandBB = rep(0, 4)))
expect_error(mapsf:::test_ratio_input("textinput"),
             pattern = "x should be an object of class sf, sfc, sfg, bbox, SpatRaster or SpatVector" )

expect_equal(mapsf:::get_mar(), getOption("mapsf.mar"))
expect_equal(mapsf:::get_mar("sol_dark"), mapsf:::.gmapsf$themes[["sol_dark"]]$mar)

bb <- st_bbox(mtq)
expect_equal(mapsf:::bb_to_ratio(bb, mar = rep(0, 4), height = 4, width = 6), c(6, 4))
expect_equal(mapsf:::bb_to_ratio(bb, mar = rep(0,4)), c(7, 8.197))
expect_message(mapsf:::bb_to_ratio(bb, mar = rep(0,4), width = 51))
expect_equal(mapsf:::bb_to_ratio(bb, mar = rep(0,4), width = 5), c(5, 5.855))
expect_equal(mapsf:::bb_to_ratio(bb, mar = rep(0,4), height = 5.855), c(5, 5.855))

expect_equal(mf_get_ratio(mtq, theme = "base"), c(7, 8.513))


