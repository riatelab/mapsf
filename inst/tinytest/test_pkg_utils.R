if (!is.null(dev.list())) {
  dev.off()
}
expect_error(mapsf:::test_cur_plot())
mtq <- mf_get_mtq()
expect_silent(mapsf:::load_default_theme())
m <- sf::st_transform(mtq, "EPSG:4326")
mf_map(m)
expect_error(mf_scale())
