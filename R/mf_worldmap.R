#' @title Plot a point on a world map
#' @description Plot a point on a world map.
#' @eval my_params("xfull")
#' @param lon longitude
#' @param lat latitude
#' @param ... further parameters to pass to points (cex, pch, col...).
#' @return No return value, a world map is displayed.
#' @export
#' @note The main part of the code is stolen from @fzenoni
#' (\url{https://gist.github.com/fzenoni/ef23faf6d1ada5e4a91c9ef23b0ba2c1}).
#' @examples
#' mtq <- mf_get_mtq()
#' mf_worldmap(mtq)
#' mf_worldmap(lon = 24, lat = 39)
mf_worldmap <- function(x, lon, lat, ...) {
  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  on.exit(par(op))
  ops <- list(...)
  ops$pch <- ifelse(is.null(ops$pch), 17, ops$pch)
  ops$col <- ifelse(is.null(ops$col), "red", ops$col)
  ops$x <- 0
  ops$y <- 0

  suppressMessages({
    suppressWarnings({
      if (!missing(x)) {
        co <- st_coordinates(st_centroid(
          st_as_sfc(st_bbox(st_transform(x, 4326)))
        ))
        lon <- co[1]
        lat <- co[2]
      }
      plot(orthomap(lon, lat, disc = TRUE),
        col = "lightblue",
        border = "lightblue",
        bg = .gmapsf$args$bg
      )
      plot(orthomap(lon, lat, disc = FALSE),
        add = TRUE,
        col = "grey60", border = "grey50", lwd = 1
      )
    })
  })
  do.call(points, ops)
  return(invisible(NULL))
}


# https://gist.github.com/fzenoni/ef23faf6d1ada5e4a91c9ef23b0ba2c1
orthomap <- function(lon, lat, disc) {
  ortho <- paste0(
    "+proj=ortho +lat_0=", lat, " +lon_0=", lon,
    " +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs"
  )

  # Define the polygon that will help you finding the "blade"
  # to split what lies within and without your projection
  circle <- st_point(x = c(0, 0))
  circle <- st_buffer(circle, dist = 6371000)
  circle <- st_sfc(circle, crs = ortho)

  if (disc) {
    return(circle)
  }

  # Project this polygon in lat-lon
  circle_longlat <- st_transform(circle, crs = 4326)

  # circle_longlat cannot be used as it is
  # You must decompose it into a string with ordered longitudes
  # Then complete the polygon definition to cover the hemisphere
  if (lat != 0) {
    circle_longlat <- st_boundary(circle_longlat)
    circle_coords <- st_coordinates(circle_longlat)[, c(1, 2)]
    circle_coords <- circle_coords[order(circle_coords[, 1]), ]
    circle_coords <- circle_coords[!duplicated(circle_coords), ]
    # Rebuild line
    circle_longlat <- st_sfc(st_linestring(circle_coords), crs = 4326)
    if (lat > 0) {
      rectangle <- list(rbind(
        circle_coords,
        c(X = 180, circle_coords[nrow(circle_coords), "Y"]),
        c(X = 180, Y = 90),
        c(X = -180, Y = 90),
        c(X = -180, circle_coords[1, "Y"]),
        circle_coords[1, c("X", "Y")]
      ))
      rectangle <- st_sfc(st_polygon(rectangle), crs = 4326)
    } else {
      rectangle <- list(rbind(
        circle_coords,
        c(X = 180, circle_coords[nrow(circle_coords), "Y"]),
        c(X = 180, Y = -90),
        c(X = -180, Y = -90),
        c(X = -180, circle_coords[1, "Y"]),
        circle_coords[1, c("X", "Y")]
      ))
      rectangle <- st_sfc(st_polygon(rectangle), crs = 4326)
    }
    circle_longlat <- st_union(
      st_make_valid(circle_longlat),
      st_make_valid(rectangle)
    )
  }

  mini_world <- st_read(system.file("gpkg/world.gpkg", package = "mapsf"),
    layer = "country", quiet = TRUE
  )
  mini_world <- st_geometry(mini_world)
  visible <- st_intersection(
    st_make_valid(mini_world),
    st_buffer(circle_longlat, -0.09)
  )
  visible <- st_transform(visible, crs = ortho)
  return(visible)
}
