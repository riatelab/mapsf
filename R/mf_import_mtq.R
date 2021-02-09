#' Get the 'mtq' dataset
#' @name mf_get_mtq
#' @description Import the mtq dataset.
#' @return an sf object
#' @export
#' @details This a wrapper around
#' \code{st_read(system.file("gpkg/mtq.gpkg", package = "mapsf"),quiet = TRUE)}.
#' @importFrom sf st_read
#' @examples
#' mtq <- mf_get_mtq()
mf_get_mtq <- function() {
  st_read(system.file("gpkg/mtq.gpkg", package = "mapsf"), quiet = TRUE)
}
