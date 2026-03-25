expect_silent(mf_map(mtq, c("POP", "STATUS"), type = "prop_typo", add = FALSE))

mf_map(mtq)
expect_silent(mf_map(mtq, c("POP", "STATUS"), type = "prop_typo"))

mtq[6, "STATUS"] <- NA
expect_silent(
  mf_map(
    x = mtq, var = c("POP", "STATUS"), type = "prop_typo",
    inches = .35, border = "tomato4",
    val_max = 90000, symbol = "square", col_na = "grey", pal = "Dynamic",
    lwd = 2,
    leg_pos = c("bottomright"),
    leg_title = c("Population", "Municipality\nstatus"),
    leg_title_cex = c(0.9, 0.9),
    leg_val_cex = c(.7, .7),
    val_order = c("Prefecture", "Sub-prefecture", "Simple municipality"),
    leg_no_data = "No dada",
    leg_frame = c(TRUE, TRUE),
    add = TRUE)
)


mf_map(mtq)
expect_silent(mf_map(mtq_l, c("fij", "sj"), "prop_typo", add = TRUE))
mtq_l[2, "sj"] <- NA
expect_silent(mf_map(mtq_l, c("fij", "sj"), "prop_typo",
                     val_max = 5000, add = FALSE, col_na = "red"))


expect_message(mf_map(mtq, c("POP", "STATUS"), type = "prop_typo",
                      leg_pos = c("top", "bottom")))
expect_message(mf_map(mtq_l, c("fij", "sj"), type = "prop_typo",
                      leg_pos = c("top", "bottom")))
