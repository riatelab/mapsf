expect_silent(mf_get_ratio(mtq, height = 6))
expect_silent(mf_get_ratio(mtq, width = 7))
expect_silent(mf_get_ratio(mtq))
expect_equal(mf_get_ratio(mtq, theme = "base"), c(7, 8.513))
expect_silent(mf_get_ratio(mtq, width = 3, expandBB = c(0, 0, 0, .3),
                           theme = "sol_dark"))
expect_silent(mf_get_ratio(sf::st_transform(mtq, 4326)))
r <- terra::rast(system.file("ex/elev.tif", package = "terra"))
expect_silent(mf_get_ratio(r))
