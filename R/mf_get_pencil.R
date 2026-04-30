#' @title Get a pencil layer from polygons
#' @name mf_get_pencil
#' @description Create a pencil layer. This function transforms a POLYGON or
#' MULTIPOLYGON sf object into a MULTILINESTRING one.
#' @param x an sf object, a simple feature collection (POLYGON or MULTIPOLYGON).
#' @param size density of the penciling. Median number of points used to build
#' the MULTILINESTRING.
#' @param buffer buffer around each polygon. This buffer (in map units) is used
#' to take sample points. A negative value adds a margin between the penciling
#' and the original polygons borders
#' @param lefthanded if TRUE the penciling is done left-handed style.
#' @param clip if TRUE, the penciling is cut by the original polygon.
#' @return A MULTILINESTRING sf object is returned.
#' @examples
#' mtq <- mf_get_mtq()
#' mtq_pencil <- mf_get_pencil(x = mtq, clip = FALSE)
#' mf_map(mtq)
#' mf_map(mtq_pencil, add = TRUE)
#' @export
mf_get_pencil <- function(x, size = 100, buffer = 0, lefthanded = TRUE,
                          clip = FALSE) {
  a <- median(st_area(st_set_crs(x, NA)))
  size <- size * size
  . <- lapply(st_geometry(x), makelines,
    size = size, buffer = buffer,
    lefthanded = lefthanded, a = a, clip = clip
  )
  . <- st_sfc(do.call(rbind, .))
  if (length(.) < nrow(x)) {
    stop(
      paste0(
        "Try a smaller value for 'buffer' ",
        "or a larger vaue for 'size'"
      ),
      call. = FALSE
    )
  }
  . <- st_sf(geometry = ., x[, , drop = TRUE], sf_column_name = "geometry")
  . <- st_set_crs(., st_crs(x))
  . <- st_cast(., "MULTILINESTRING")
  return(.)
}

makelines <- function(x, size, buffer, lefthanded, a, clip) {
  size <- round(sqrt(as.numeric(st_area(x) * size / a)), 0)
  if (size <= 10) {
    size <- 10
  }
  xbuf <- st_buffer(st_sfc(x), buffer)

  pt <- st_sample(xbuf, size = size, exact = FALSE)

  if (lefthanded) {
    pt <- st_sf(pt, x = st_coordinates(pt)[, 2] +
      st_coordinates(pt)[, 1])
  } else {
    pt <- st_sf(pt, x = st_coordinates(pt)[, 2] -
      st_coordinates(pt)[, 1])
  }
  pt <- st_combine(pt[order(pt$x), ])

  if (isTRUE(clip)) {
    pt <- st_intersection(st_cast(pt, "LINESTRING"), x)
  } else {
    pt <- st_intersection(st_cast(pt, "LINESTRING"), xbuf)
  }
  return(pt)
}
