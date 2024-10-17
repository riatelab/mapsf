#' @title Get a border layer from polygons
#' @description This function extracts borders between contiguous polygons.
#' @name mf_get_borders
#' @param x an sf object of POLYGONS, using a projected CRS
#' @note
#' If the polygon layer contains topology errors (such as contiguous
#' polygons not sharing exactly the same boundary) the function may not return
#' all boundaries correctly. It is possible to use `st_snap()` or other
#' functions to try and correct these errors.
#' @return An sf object (MULTILINESTRING) of borders is returned.
#' @md
#' @examples
#' mtq <- mf_get_mtq()
#' mtq_b <- mf_get_borders(mtq)
#' mf_map(mtq)
#' mf_map(mtq_b, col = 1:5, lwd = 4, add = TRUE)
#' @export
mf_get_borders <- function(x) {
  if (get_geom_type(x) != "POLYGON") {
    stop(paste0('"x" should be a POLYGON sf object'), call. = FALSE)
  }
  if (sf::st_is_longlat(x)) {
    stop("This feature does not work on unprojected (long/lat) layers.",
         call. = FALSE)
  }
  oag <- sf::st_agr(x)
  x <- sf::st_set_agr(x, "constant")
  border <- sf::st_collection_extract(sf::st_intersection(x, x), "LINESTRING")
  border <- sf::st_set_agr(border, c(oag, oag))
  return(border)
}
