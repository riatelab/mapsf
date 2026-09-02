
## mf_theme
expect_silent(mf_theme())
expect_error(mf_theme("iceberg"))
expect_silent(mf_theme("sol_light"))
expect_error(mf_theme("NoT a thEmE"))
expect_silent(mf_theme(x = list(
  mar = c(1, 1, 3, 1),
  title_tab = FALSE,
  title_pos = "left",
  title_inner = FALSE,
  title_line = 2,
  title_cex = 1.5,
  title_font = 2,
  title_banner = FALSE,
  frame = "figure",
  frame_lwd = 1,
  frame_lty = 1,
  foreground = "#fbfbfb",
  background = "grey75",
  highlight = "#0f5027",
  pal_quali = "Dark 3",
  pal_seq = "Greens"
)))

expect_silent(mf_map(mtq, "POP", "prop", leg_frame = TRUE, add = FALSE))
expect_silent(mf_theme(NULL))
