suppressPackageStartupMessages(library("terra"))

a <- rast(system.file("ex/elev.tif", package = "terra"))
b <- rast(system.file("ex/logo.tif", package = "terra"))
d <-  classify(a, c(140,450,500, 550))
levels(d) <- data.frame(ID = 0:2, elevation = c("low", "high", "super high"))


# input type
expect_error(mf_raster(x = "x"))

# multiband
expect_silent(mf_raster(b))
expect_silent(mf_raster(b, add = TRUE))

# internals
# get_the_raster_pal
expect_equal(mapsf:::get_the_raster_pal("Reds 2", 6, 1, TRUE),
             hcl.colors(6, "Reds 2", alpha = 1, rev = TRUE))
expect_error(mapsf:::get_the_raster_pal("Jaune", 6, 1, TRUE))
expect_equal(mapsf:::get_the_raster_pal(c("red", "blue"), 6, 1, TRUE),
             c("#FF0000FF", "#CC0033FF", "#990066FF", "#650099FF",
               "#3200CCFF", "#0000FFFF"))
# get_continuous_pal
pp <- mapsf:::get_continuous_pal(c(0,10,20), pal = c("red", "white", "blue"))
expect_equal(pp[c(1,500,1000)], c("#FF0000", "#FFFFFF", "#0000FF"))

# type
expect_error(mf_raster(a, type = "coninuous"))

# interval
expect_silent(mf_raster(a, "interval"))

# continuous
expect_silent(mf_raster(a))
expect_error(mf_raster(a, breaks = c(141,500,547), pal = 'red'))
expect_silent(mf_raster(a,breaks = c(141,400,547),
                        pal = c('red', 'black', 'yellow')))

# classes
expect_silent(mf_raster(d, "classes"))
expect_silent(mf_raster(d, "classes", pal = "Burg",
                        val_order = rev(c("low", "high", "super high"))))
expect_silent(mf_raster(a, "classes", pal = "Burg"))
