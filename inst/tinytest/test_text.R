pos <- c('center', 'topleft', 'top', 'topright', 'right', 'bottomright',
         'bottom',  'left', 'bottomleft')
# input
expect_silent({
  mf_map(mtq)
  mf_text(x = mtq[1, ])
  mf_text(x = sf::st_geometry(mtq[2, ]))
  mf_text(x = c(731029.56079219, 1626948.29177051))
  for (i in pos[-1]) {
    mf_text(x = i)
  }
})

# position
expect_silent({
  mf_map(mtq)
  for (i in pos) {
    mf_text(x = mtq[4, ], pos = i, offset = 5)
  }
})

# alignement
expect_silent({
  mf_map(mtq)
  for (i in pos) {
    mf_text(x = mtq[5, ], pos = i, txt = "2 lines of\ntext", align = "left", offset = 1)
  }
  mf_map(mtq)
  for (i in pos) {
    mf_text(x = mtq[26, ], pos = i, txt = "2 lines of\ntext", align = "center", offset = 1)
  }
  mf_map(mtq)
  for (i in pos) {
    mf_text(x = mtq[27, ], pos = i, txt = "2 lines of\ntext", align = "right", offset = 1)
  }
})

# lines
expect_silent({
  mf_map(mtq)
  for (i in pos) {
    mf_text(x = mtq[5, ], pos = i, txt = "2 lines of\ntext", align = "left", offset = 10, line = 1)
  }
  mf_map(mtq)
  for (i in pos) {
    mf_text(x = mtq[4, ], pos = i, txt = "2 lines of\ntext", align = "center", offset = 15, line = 2, clockwise = TRUE)
  }
  mf_map(mtq)
  for (i in pos) {
    mf_text(x = mtq[4, ], pos = i, txt = "2 lines of\ntext", align = "center", offset = 15, line = 2, clockwise = FALSE)
  }
  mf_map(mtq)
  for (i in pos) {
    mf_text(x = mtq[4, ], pos = i, txt = "2 lines of\ntext", align = "center", offset = 15, line = 3, clockwise = TRUE)
  }
  mf_map(mtq)
  for (i in pos) {
    mf_text(x = mtq[4, ], pos = i, txt = "2 lines of\ntext", align = "center", offset = 15, line = 3, clockwise = FALSE)
  }
})

# no arrow
expect_silent({
  mf_map(mtq)
  for (i in pos) {
    mf_text(x = mtq[4, ], pos = i, offset = 15, line = 1, arrow = FALSE)
  }
})

# wrong pos
expect_warning({
  mf_map(mtq)
  mf_text(x = mtq[4, ], pos = "West", offset = 15, line = 1, arrow = FALSE)
})


# halo
expect_silent({
  mf_map(mtq)
  for (i in pos) {
    mf_text(x = mtq[4, ], pos = i, offset = 5, halo = TRUE, col_halo = "green")
  }
})

# box
expect_silent({
  mf_map(mtq)
  for (i in pos) {
    mf_text(x = mtq[4, ], pos = i, offset = 15, box = TRUE, col_box = "green")
  }
})
