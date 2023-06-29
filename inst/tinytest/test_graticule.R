mtq <- mf_get_mtq()
mf_map(mtq, expandBB = rep(.2, 4))
expect_silent(
  mf_graticule(
    mtq,
    pos = c("top", "left", "bottom", "right"),
    add = TRUE
  )
)
expect_silent(
  mf_graticule(
    mtq,
    pos = c("top", "left", "bottom", "right"),
    label = FALSE,
    add = FALSE
  )
)

g <- mf_graticule(mtq, add = TRUE)
expect_inherits(g, "sf")
expect_length(sf::st_geometry(g), 9)
