expect_silent(
  mf_map(
    x = mtq, var = "STATUS", type = "typo",
    pal = c("red", "blue", "yellow"), lwd = 1.1,
    val_order = c("Prefecture", "Sub-prefecture", "Simple municipality"),
    col_na = "green", border = "brown",
    leg_pos = "bottomleft",
    leg_title = "Status", leg_title_cex = 1.1,
    leg_val_cex = 1, leg_no_data = "No data",
    leg_frame = TRUE, add = FALSE)
)

mtq[6, "STATUS"] <- NA
expect_silent(mf_map(x = mtq, var = "STATUS", type = "typo"))
expect_silent(mf_map(x = mtq_p, var = "STATUS", type = "typo"))
expect_silent(mf_map(x = mtq_l, var = "sj", type = "typo"))
