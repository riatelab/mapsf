mtq <- mf_get_mtq()
mf_map(mtq)
expect_silent(mf_map(st_cast(mtq, "MULTILINESTRING"), var = "POP", type = "grad"))
expect_silent(mf_map(st_cast(mtq, "MULTILINESTRING"), var = "POP", type = "grad", add = FALSE))
expect_silent(mf_map(mtq, var = "POP", type = "grad", add = FALSE))
expect_error(mf_map(st_cast(mtq, "MULTILINESTRING"), var = "POP", type = "grad",
                    lwd = c(1,2)))
expect_error(mf_map(mtq, var = "POP", type = "grad",
                    cex = c(1,2)))
