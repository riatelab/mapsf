mf_map(mtq)
expect_silent(mf_label(x = mtq[1,], var = "LIBGEO", halo = FALSE, cex = 0.8,
                       overlap = FALSE, lines = TRUE))
expect_silent(mf_label(x = mtq, var = "LIBGEO", halo = TRUE, cex = 0.8,
         overlap = FALSE, lines = TRUE))
expect_silent(mf_label(x = mtq, var = "LIBGEO", halo = TRUE,
                       cex = 0.8, col = sample(1:5, nrow(mtq), replace = TRUE),
                       overlap = FALSE, lines = TRUE))
expect_silent(mf_label(x = mtq, var = "LIBGEO", halo = TRUE,
                       cex = 0.8, col = sample(1:5, nrow(mtq), replace = TRUE),
                       overlap = FALSE, lines = TRUE, pos = 3, font = 3))
expect_silent(mf_label(x = mtq, var = "LIBGEO", add = FALSE))
