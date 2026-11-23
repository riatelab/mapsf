expect_silent(mf_png(mtq, height = 600,
                    filename = paste0(tempfile(), ".png")))
dev.off()

expect_silent(mf_png(mtq, filename = paste0(tempfile(), ".png")))
dev.off()

expect_silent(mf_svg(mtq, height = 7,
                     filename = paste0(tempfile(), ".svg")))
dev.off()

expect_silent(mf_svg(mtq, height = 7,
                     filename = paste0(tempfile(), ".svg"),
                     svglite = FALSE))
dev.off()
