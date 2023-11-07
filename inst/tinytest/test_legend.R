mtq <- mf_get_mtq()
expect_error(test_cur_plot())
mf_map(mtq)
expect_silent(mf_legend(type = "prop", pos = "topright",
                        val = c(1, 5, 10), inches = .3))
expect_silent(mf_legend(type = "choro", pos = "bottomright",
                        val = c(10, 20, 30, 40, 50),
                        pal = hcl.colors(4, "Reds 2")))
expect_silent(mf_legend(type = "typo", pos = "topleft",
                        val = c("A", "B", "C", "D"),
                        pal = hcl.colors(4, "Dynamic")))
expect_silent(mf_legend(type = "symb", pos = "bottomleft",
                        val = c("A", "B", "C"),
                        pch = 21:23, cex = c(1, 2, 2),
                        pal = hcl.colors(3, "Dynamic")))
expect_silent(mf_legend(type = "grad_line", pos = "top",
                        val = c(1, 2, 3, 4, 10, 15),
                        lwd = c(0.2, 2, 4, 5, 10)))
expect_silent(mf_legend(type = "prop_line", pos = "bottom", lwd = 20,
                        val = c(5, 50, 100)))
expect_error(suppressWarnings(mf_legend(type = "prop_line",
                                        pos = "interactive", lwd = 20,
                       val = c(5, 50, 100))))


expect_error(mf_legend(type = "grad_line", pos = "NA",
                        val = c(1, 2, 3, 4, 10, 15),
                        lwd = c(0.2, 2, 4, 5, 10)))

expect_silent(mf_legend(type = "grad_line",
                        pos = c(737788.682080213, 1628182.17278935),
                        val = c(1, 2, 3, 4, 10, 15),
                        lwd = c(0.2, 2, 4, 5, 10), frame = TRUE))
expect_silent(mf_legend(type = "grad_line", pos = NA,
                        val = c(1, 2, 3, 4, 10, 15),
                        lwd = c(0.2, 2, 4, 5, 10)))
expect_silent(mf_legend(type = "prop_line", pos = NA,
                        lwd = 20,
                        val = c(5, 50, 100)))
expect_silent(mf_legend(type = "prop_line",
                        pos = c(737788.682080213, 1628182.17278935),
                        lwd = 20,
                        val = c(5, 50, 100),
                        frame = TRUE))


## test deprecation
expect_warning(mf_legend_c(val = c(1, 2, 3, 4),
                           pal = c("red1", "red3", "red4")))
expect_warning(mf_legend_t(val = c("type A", "type B"),
                           pal = c("navy", "tomato")))
expect_warning(mf_legend_gl(lwd = c(0.2, 2, 4, 5, 10),
                            val = c(1, 2, 3, 4, 10.2, 15.2)))
expect_warning(mf_legend_s(val = c("Type C", "Type D"),
                           pal = c("cyan", "plum"),
                           pt_pch = c(21, 23), pt_cex = c(1, 2)))
expect_warning(mf_legend_p(val = c(1, 20, 100),
                           col = "red", inches = .3))
expect_warning(mf_legend_pl(lwd = 20, val = c(5, 10, 50, 100)))

dev.off()
expect_equal(mf_get_leg_pos(mtq, 1), "bottomleft")
expect_equal(mf_get_leg_pos(mtq[1,], 2), c("topleft", "top"))
mf_map(mtq)
expect_equal(mf_get_leg_pos(mtq[1,], 2), c("topright", "right"))

expect_error(mapsf:::interleg())
