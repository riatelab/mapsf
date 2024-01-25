mtq <- mf_get_mtq()
mf_map(mtq)
expect_silent(mf_symb_choro(mtq, var = c("STATUS", "MED"),
                            leg_pos = c("top", "topright")))
