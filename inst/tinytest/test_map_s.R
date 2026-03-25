mf_map(mtq)
expect_silent(mf_map(x = mtq, var = "STATUS", type = "symb",add = FALSE))
expect_message(mf_map(x = mtq, var = "STATUS", type = "symb",
                      pch = 20:21, cex = c(2, 2)))
expect_message(mf_map(x = mtq_l, var = "sj", type = "symb"))
