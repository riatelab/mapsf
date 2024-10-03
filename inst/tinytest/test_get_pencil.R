mtq <- mf_get_mtq()
a <- mf_get_pencil(mtq, buffer = 100)
b <- mf_get_pencil(mtq, buffer = -100, lefthanded = FALSE, clip = FALSE)
expect_true(methods::is(st_geometry(a), "sfc_MULTILINESTRING"))
expect_true(methods::is(st_geometry(b), "sfc_MULTILINESTRING"))
expect_error(mf_get_pencil(mtq, size = 10, buffer = 10000000, clip = T))
mtq$MED[1:3] <- NA
expect_silent(mf_map(mf_get_pencil(mtq,100), var = 'MED', type = 'choro'))

