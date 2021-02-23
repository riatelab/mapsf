# init
mtq <- mf_get_mtq()

expect_silent(mf_shadow(mtq))
expect_silent(mf_shadow(mtq, add = TRUE))
