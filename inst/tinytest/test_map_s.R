mtq <- mf_get_mtq()
mf_map(mtq)
expect_message(mf_map(x = mtq, var = "STATUS", type = "symb",
                      pch = 20:21, cex = c(2, 2)))
