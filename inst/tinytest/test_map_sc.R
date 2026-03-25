mf_map(mtq)
expect_silent(mf_map(mtq, var = c("STATUS", "MED"), type = "symb_choro",
                            leg_pos = c("top")))
expect_message(mf_map(mtq, var = c("STATUS", "MED"), type = "symb_choro",
                     leg_pos = c("top", "topright")))
expect_message(mf_map(mtq_l, var = c("sj", "sj"), type = "symb_choro"))
expect_message(mf_map(mtq, var = c("STATUS", "MED"), type = "symb_choro",
                      pch = 1:2, cex = 1:2))
expect_silent(mf_map(mtq, var = c("STATUS", "MED"), type = "symb_choro",
                     leg_pos = "top", add = FALSE))
