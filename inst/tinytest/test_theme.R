
## mf_theme
expect_silent(mf_theme())
expect_error(mf_theme("NoT a thEmE"))
expect_silent(mf_theme(x = list(
  name = "custom",
  bg = "black",
  fg = "yellow",
  mar = c(5, 5, 5, 5),
  tab = TRUE,
  pos = "right",
  inner = FALSE,
  line = 4,
  cex = 2,
  font = 3
)))
expect_silent(mf_theme(bg = "green"))
