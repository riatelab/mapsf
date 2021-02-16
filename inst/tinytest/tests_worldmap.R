mtq <- mf_get_mtq()
expect_silent(mf_worldmap(mtq))
expect_silent(mf_worldmap(lon = 5, lat = 34))
