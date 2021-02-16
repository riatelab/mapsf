mtq <- mf_get_mtq()
mf_map(mtq)
expect_silent(mf_map(st_cast(mtq, "MULTILINESTRING"), var = "POP", type = "grad"))
