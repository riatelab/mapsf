#' @title Plot a point on a world map
#' @description Plot a point on a world map.
#' @eval my_params("xfull")
#' @param lon longitude
#' @param lat latitude
#' @param water_col color of the water
#' @param land_col color of the land
#' @param border_col color of the borders
#' @param border_lwd width of the borders
#' @param ... further parameters related to the plotted point aspect
#' (cex, pch, col...)
#' @return No return value, a world map is displayed.
#' @export
#' @note The main part of the code is stolen from @fzenoni
#' (\url{https://gist.github.com/fzenoni/ef23faf6d1ada5e4a91c9ef23b0ba2c1}).
#' @examples
#' mtq <- mf_get_mtq()
#' mf_worldmap(mtq)
#' mf_worldmap(lon = 24, lat = 39)
#' mf_worldmap(
#'   lon = 106, lat = 26,
#'   pch = 4, lwd = 3, cex = 2, col = "tomato4",
#'   water_col = "#232525", land_col = "#A9B7C6",
#'   border_col = "white", border_lwd = 1
#' )
mf_worldmap <- function(x, lon, lat, water_col = "lightblue",
                        land_col = "grey60", border_col = "grey40",
                        border_lwd = .8, ...) {
  ops <- list(...)
  ops$pch <- ifelse(is.null(ops$pch), 17, ops$pch)
  ops$col <- ifelse(is.null(ops$col), "red", ops$col)
  ops$x <- 0
  ops$y <- 0

  if (!missing(x)) {
    co <- st_coordinates(
      st_transform(st_centroid(st_as_sfc(st_bbox(x))), "EPSG:4326")
    )
    lon <- co[1]
    lat <- co[2]
  }
  lo <- orthomap(lon, lat)
  mf_map(lo$ocean,
    col = water_col,
    border = water_col,
    bg = getOption("mapsf.bg")
  )
  mf_map(lo$land,
    add = TRUE,
    col = land_col,
    border = border_col,
    lwd = border_lwd
  )

  do.call(points, ops)
  return(invisible(NULL))
}


#' @import s2
orthomap <- function(lon, lat) {
  g <- as_s2_geography(TRUE)
  co <- s2_data_countries()
  oc <- s2_difference(g, s2_union_agg(co)) # oceans
  co <- s2_difference(co, s2_union_agg(oc)) # land

  # visible half
  b <- s2_buffer_cells(as_s2_geography(paste0("POINT(", lon, " ", lat, ")")),
    distance = 9800000
  )

  # proj
  prj <- paste0("+proj=ortho +lat_0=", lat, " +lon_0=", lon)

  # visible land
  cov <- s2_intersection(b, co)
  cov <- st_transform(st_as_sfc(cov), prj)
  cov <- cov[!st_is_empty(cov)]
  cov <- suppressWarnings(st_collection_extract(cov, "POLYGON"))
  cov <- st_cast(cov, "MULTIPOLYGON")


  # visible ocean
  ocv <- s2_intersection(b, oc)
  ocv <- st_transform(st_as_sfc(ocv), prj)
  ocv <- ocv[!st_is_empty(ocv)]
  ocv <- suppressWarnings(st_collection_extract(ocv, "POLYGON"))
  ocv <- st_cast(ocv, "MULTIPOLYGON")

  return(list(ocean = ocv, land = cov))
}
