if (requireNamespace("tinytest", quietly = TRUE)) {
  suppressPackageStartupMessages(library(sf))
  mtq <- st_read(system.file("gpkg/mtq.gpkg", package = "mapsf"),
                   layer = 'polygons', quiet = TRUE)
  mtq_p <- st_read(system.file("gpkg/mtq.gpkg", package = "mapsf"),
                 layer = 'points', quiet = TRUE)
  mtq_l <- st_read(system.file("gpkg/mtq.gpkg", package = "mapsf"),
                   layer = 'lines', quiet = TRUE)
  tinytest::test_package("mapsf")
}
