mtq <- mf_get_mtq()
expect_silent(mf_prop_choro(mtq, c("POP", "MED"), add = FALSE))

mf_map(mtq)
expect_silent(mf_prop_choro(mtq, c("POP", "MED")))

mf_map(mtq)
mtq[6, "MED"] <- NA
expect_silent(
  mf_prop_choro(
    x = mtq, var = c("POP", "MED"), inches = .35, border = "tomato4",
    val_max = 90000, symbol = "circle", col_na = "grey", pal = "Cividis",
    breaks = "equal", nbreaks = 4, lwd = 4,
    leg_pos = c("bottomright", "bottomleft"),
    leg_title = c("Population", "Median Income"),
    leg_title_cex = c(0.8, 1),
    leg_val_cex = c(.7, .9),
    leg_val_rnd = c(0,0),
    leg_no_data = "No data",
    leg_frame = c(TRUE, TRUE),
    add = TRUE
  )
)
