mtq <- mf_get_mtq()
mf_init(mtq)
expect_silent(mf_background(system.file("img/background.jpg",
                                        package = "mapsf")))
expect_silent(mf_background(system.file("help/figures/logo.png",
                                        package = "mapsf")))
