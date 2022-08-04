mtq <- mf_get_mtq()



expect_silent(mf_export(mtq, height = 600,
                        filename = paste0(tempfile(), ".png")))
dev.off()
expect_silent(mf_export(mtq, filename = paste0(tempfile(), ".png")))
dev.off()

expect_silent(mf_export(mtq, paste0(tempfile(), ".png"),
                        width = 800, height = 800))
dev.off()

expect_silent(mf_export(mtq, filename = paste0(tempfile(), ".svg"),
                        width = 6, height = 6))
dev.off()
expect_silent(mf_export(mtq, theme = "darkula",
                        filename = paste0(tempfile(), ".svg")))
dev.off()
expect_message(mf_export(mtq, width = 51,
                         filename = paste0(tempfile(), ".svg")))
dev.off()

expect_silent(mf_export(mtq, height = 7,
                        filename = paste0(tempfile(), ".svg")))
dev.off()

r <- terra::rast(system.file("ex/elev.tif", package="terra"))
expect_message(mf_export(r))
mf_raster(r, add = T)
dev.off()

expect_message(mf_export(mtq, height = 600,export = "png",
                         filename = paste0(tempfile(), ".png")))
dev.off()

expect_message(mf_export(st_transform(mtq, "epsg:4326"),
                         filename = paste0(tempfile(), ".png")))
dev.off()
