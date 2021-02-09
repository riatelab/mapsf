## mf_get_pal
expect_silent(mf_get_pal(n = 5, palette = 'Dynamic'))
expect_silent(mf_get_pal(n = c(6, 5), palette = c("Mint", 'Reds 2')))
expect_silent(mf_get_pal(n = c(6, 5), palette = c("Mint", 'Reds 2'), neutral = "grey"))
