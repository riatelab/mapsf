mtq <- mf_get_mtq()
mf_map(mtq)
expect_silent(mf_annotation(x = c(711167.8,1614764 ),
              txt = "Look!\nImportant feature\nhere!",
              pos = "bottomleft", cex = 1.2, font = 2,
              halo = TRUE, s = 1.5 ))
expect_silent(mf_annotation(x = mtq[20, ],
              txt = "This is less\nimportant",
              cex = .7, font = 3, s = 1.3 ))
expect_silent(mf_annotation(x = mtq[20, ], pos = "topleft", s = 0.9,
                            txt = "This"))
expect_silent(mf_annotation(x = mtq[20, ], pos = "bottomright",
                            txt = "This"))


expect_silent(mf_annotation(x = mtq[20, ], pos = "topright",
                            txt = "This", halo = TRUE))

expect_silent(mf_annotation(x = mtq[20, ], pos = "topleft",
                            txt = "This", halo = TRUE))
expect_silent(mf_annotation(x = mtq[20, ], pos = "bottomleft",
                            txt = "This", halo = FALSE))
expect_silent(mf_annotation(x = mtq[20, ], pos = "bottomright",
                            txt = "This", halo = TRUE))
