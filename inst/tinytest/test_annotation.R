mtq <- mf_get_mtq()
mf_map(mtq)
expect_silent(mf_annotation(x = c(711167.8,1614764 ),
              txt = "Look!\nImportant feature\nhere!",
              pos = "bottomleft", cex = 1.2, font = 2,
              halo = TRUE, s = 1.5 ))
expect_silent(mf_annotation(x = mtq[20, ],
              txt = "This is less\nimportant",
              cex = .7, font = 3, s = 1.3 ))
