# init
mtq <- mf_get_mtq()

expect_silent(mf_init(mtq, theme = "darkula", shadow = TRUE))
expect_silent(mf_init(mtq, export = "png", height = 600,
                      filename = tempfile()))
dev.off()
expect_silent(mf_init(mtq, export = "png", filename = tempfile()))
dev.off()
expect_silent(mf_init(mtq, theme = "darkula", export = "svg",
                      filename = tempfile()))
dev.off()
expect_message(mf_init(mtq, export = "svg", width = 51,
                       filename = tempfile()))
dev.off()

expect_silent(mf_init(mtq, export = "svg", height = 7,
                       filename = tempfile()))
dev.off()

