# init
mtq <- mf_get_mtq()
expect_error(mf_frame())
mf_map(mtq)
expect_silent(mf_frame(extent = "map", lwd = 1))
expect_silent(mf_frame(extent = "figure", lwd = 1))
