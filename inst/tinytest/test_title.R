mtq <- mf_get_mtq()

mf_map(mtq)


expect_silent(mf_title(pos = "left", inner = FALSE, tab = FALSE))
expect_silent(mf_title(pos = "center", inner = FALSE, tab = FALSE))
