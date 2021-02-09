mtq <- mf_get_mtq()
expect_silent(mf_choro(
  x = mtq, var = "MED", col_na = "grey", pal = "Cividis",
  breaks = "quantile", nbreaks = 4, border = "white",
  lwd = .5, leg_pos = "bottomleft",
  leg_title = "Median Income", leg_title_cex = 1.1,
  leg_val_cex = 1, leg_val_rnd = -2, leg_no_data = "No data",
  leg_frame = TRUE
))

mtq[6, "MED"] <- NA
expect_silent(mf_choro(x = mtq, var = "MED"))


expect_silent(mf_choro(sf::st_cast(mtq, "MULTIPOINT"), "MED"))
expect_silent(mf_choro(sf::st_cast(mtq, "MULTILINESTRING"), "MED"))
