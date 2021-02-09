mtq <- mf_get_mtq()
mf_map(mtq)
expect_silent(mf_prop(x = mtq, var = "POP", inches = .4, symbol = "circle",
                       val_max = 90000,
                       col = "tomato1", border = "blue", lwd = 1,
                       leg_pos = "right", leg_title = "Population",
                       leg_title_cex = 1, leg_val_cex = .8, leg_val_rnd = 0,
                       leg_frame = TRUE, add = TRUE))
expect_silent(mf_prop(x = mtq, var = "POP", add = FALSE))
expect_silent(mf_prop(x = mtq, var = "POP", inches = .2, symbol = "circle",
                       val_max = 30000,
                       col = "tomato1", lwd = 1,
                       leg_pos = "right", leg_title = "Population",
                       leg_title_cex = 1, leg_val_cex = .8, leg_val_rnd = 0,
                       leg_frame = TRUE))
expect_silent(mf_prop(sf::st_cast(mtq, "MULTILINESTRING"), "POP"))
