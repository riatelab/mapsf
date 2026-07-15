mf_map(mtq)
expect_silent(mf_logo(system.file("img/background.jpg",
                                  package = "mapsf")))
expect_silent(mf_logo(system.file("help/figures/logo.png",
                                  package = "mapsf"),
                      pos = "topright",
                      adj = c(-10,-10), cex = 1))
expect_silent(mf_logo(system.file("help/figures/logo.png",
                                  package = "mapsf"),
                      pos = c(690000, 1625355.65627547),
                      cex = 2.5))
expect_silent(mf_logo(system.file("img/background.jpg",
                                  package = "mapsf"),
                      pos = c(690000, 1625355.65627547),
                      resize = FALSE))
expect_error(mapsf:::posinset("topleeft", pusr = graphics::par("usr"),
                              wdest = 500, hdest = 500, adj = c(0,0)))

