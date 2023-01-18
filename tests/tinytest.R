if (requireNamespace("tinytest", quietly = TRUE)) {
  suppressPackageStartupMessages(library(sf))
  tinytest::test_package("mapsf")
}
