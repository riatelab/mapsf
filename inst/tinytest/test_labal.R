mtq <- mf_get_mtq()
mf_map(mtq)
expect_silent(mf_label(x = mtq, var = "LIBGEO", halo = TRUE, cex = 0.8,
         overlap = FALSE, lines = FALSE))
