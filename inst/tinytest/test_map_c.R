expect_silent(mf_map(
  x = mtq, var = "MED", type = "choro", col_na = "grey", pal = "Cividis",
  breaks = "quantile", nbreaks = 4, border = "white",
  lwd = .5, leg_pos = "bottomleft",
  leg_title = "Median Income", leg_title_cex = 1.1,
  leg_val_cex = 1, leg_val_rnd = -2, leg_no_data = "No data",
  leg_frame = TRUE
))

mtq[6, "MED"] <- NA
expect_silent(mf_map(x = mtq, var = "MED", type = "choro"))

expect_message(mf_map(x = mtq, var = "MED", type = "choro", breaks = c(12000, 15000, 35000)))
expect_message(mf_map(x = mtq, var = "MED", type = "choro", breaks = c(12000, 15000, 19000)))

expect_silent(mf_map(mtq_p, "MED", "choro"))
expect_silent(mf_map(mtq_l, "fij", "choro"))
expect_silent(mf_map(mtq_p, "MED", "choro", pch = 4))
