mtq <- mf_get_mtq()
mf_map(mtq)
expect_silent(mf_layout(scale = TRUE, arrow = TRUE, frame = TRUE))
