mtq <- mf_get_mtq()
mob <- read.csv(system.file("csv/mob.csv", package="mapsf"))
# Select links from Fort-de-France (97209))
mob_97209 <- mob[mob$i == 97209, ]
# Create a link layer
expect_silent({mob_links <- mf_get_links(x = mtq, df = mob_97209)})
expect_equal(nrow(mob_links), 10)

expect_error({mob_links <- mf_get_links(x = mtq, df = mob_97209, x_id = "LIBGEO")})
expect_warning({mob_links <- mf_get_links(x = mtq[1:10,], df = mob_97209)})
