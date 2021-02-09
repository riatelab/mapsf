mtq <- mf_get_mtq()
expect_silent(
  mf_typo(
    x = mtq, var = "STATUS", pal = c('red', 'blue', 'yellow'), lwd = 1.1,
    val_order = c("Prefecture", "Sub-prefecture", "Simple municipality"),
    col_na = "green", border = "brown",
    leg_pos = "bottomleft",
    leg_title = "Status", leg_title_cex = 1.1,
    leg_val_cex = 1, leg_no_data = "No data",
    leg_frame = TRUE, add = FALSE)
)

mtq[6, "STATUS"] <- NA
expect_silent(mf_typo(x = mtq, var = "STATUS"))


expect_silent(mf_typo(sf::st_cast(mtq, "MULTIPOINT"), "STATUS"))
expect_silent(mf_typo(sf::st_cast(mtq, "MULTILINESTRING"), "STATUS"))
