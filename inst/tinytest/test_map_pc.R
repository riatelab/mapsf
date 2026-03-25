expect_silent(mf_map(mtq, c("POP", "MED"), type = "prop_choro", add = FALSE))
mf_map(mtq)
expect_silent(mf_map(mtq, c("POP", "MED"), type = "prop_choro"))
expect_message(mf_map(mtq, c("POP", "MED"), type = "prop_choro",
                            leg_pos = c(673434.5, 1642503.7,
                                        682727.1, 1598170.3)
))
expect_message(mf_map(mtq, c("POP", "MED"), type = "prop_choro",
                            leg_pos = c("topright",  682727.1, 1598170.3)
))


expect_message(mf_map(mtq, c("POP", "MED"), type = "prop_choro",
                            leg_pos = c(673434.5, 1642503.7, "top")
))

mf_map(mtq)
mtq[6, "MED"] <- NA
expect_silent(
  mf_map(
    x = mtq, var = c("POP", "MED"), type = "prop_choro",
    inches = .35, border = "tomato4",
    val_max = 90000, symbol = "circle", col_na = "grey", pal = "Cividis",
    breaks = "equal", nbreaks = 4, lwd = 4,
    leg_pos = c("bottomright"),
    leg_title = c("Population", "Median Income"),
    leg_title_cex = c(0.8),
    leg_val_cex = c(.7),
    leg_val_rnd = c(0, 0),
    leg_no_data = "No data",
    leg_frame = TRUE,
    add = TRUE
  )
)

expect_error(mf_map(x = mtq, var = c("POP", "MED"), type = "prop_choro",
                    leg_title = "Solo title"))

expect_message(mf_map(x = mtq_l, var = c("fij", "fij"), type = "prop_choro"))
