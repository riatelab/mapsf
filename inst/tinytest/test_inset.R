mtq <- mf_get_mtq()
mf_map(mtq)
expect_silent(mf_inset_on(x = mtq))
mf_map(mtq)
expect_silent(mf_inset_off())
expect_silent(mf_inset_on(fig = c(0, .5, 0, .5)))
mf_map(mtq)
expect_silent(mf_inset_off())


expect_silent(mf_inset_on(x = "worldmap", pos = "topleft"))
mf_worldmap(mtq)
expect_silent(mf_inset_off())

