## mf_get_pal
expect_silent(mf_get_pal(n = 5, palette = "Dynamic"))
expect_silent(mf_get_pal(n = c(6, 5), palette = c("Mint", "Reds 2")))
expect_silent(mf_get_pal(n = c(6, 5), palette = c("Mint", "Reds 2"),
                         neutral = "grey"))
expect_equal(mf_get_pal(breaks = 1:10, mid = 1, palette = c("Mint", "Viridis")),
             mf_get_pal(9, "Viridis", alpha = 1))
expect_equal(mf_get_pal(breaks = 1:10, mid = -1, palette = c("Mint", "Viridis")),
             mf_get_pal(9, "Viridis", alpha = 1))
expect_equal(mf_get_pal(breaks = 1:10, mid = 10, palette = c("Mint", "Viridis")),
             mf_get_pal(9, "Mint", alpha = 1))
expect_equal(mf_get_pal(breaks = 1:10, mid = 11, palette = c("Mint", "Viridis")),
             mf_get_pal(9, "Mint", alpha = 1))
expect_equal(mf_get_pal(breaks = 1:10, mid = 3, palette = c("Mint", "Viridis")),
             mf_get_pal(c(2, 7), c("Mint", "Viridis"), alpha = 1))
expect_error(mf_get_pal(breaks = 1:10, mid = 3, palette = c("Mint")))
expect_equal(mf_get_pal(breaks = 1:10, mid = 3.5, palette = c("Mint", "Viridis")),
             mf_get_pal(c(2, 6), c("Mint", "Viridis"), alpha = 1, neutral = "grey80"))
expect_equal(mf_get_pal(breaks = 1:10, mid = 1.5, palette = c("Mint", "Viridis")),
             c("#CCCCCCFF", mf_get_pal(8, "Viridis", alpha = 1)))
expect_equal(mf_get_pal(breaks = 1:10, mid = 9.5, palette = c("Mint", "Viridis")),
             c(mf_get_pal(8, "Mint", alpha = 1), "#CCCCCCFF"))
