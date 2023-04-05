#' @title Get borders from polygons
#' @description Get borders between polygons.
#' @name mf_get_borders
#' @param x an sf object of POLYGONS, using a projected CRS.
#' @param id name of the identifier variable in x, default to the first column.
#' (optional)
#' @return An sf object (MULTILINESTRING) of borders is returned.
#' This object has three id variables: id, id1 and id2.
#' id1 and id2 are ids of units that neighbor a border; id is the concatenation
#' of id1 and id2 (with "_" as separator).
#' @examples
#' mtq <- mf_get_mtq()
#' # Extract 3 polygons
#' m <- mtq[c(5, 29, 9), ]
#' # Get borders
#' m_borders <- mf_get_borders(x = m)
#' # Plot polygons
#' mf_map(m)
#' # Plot borders
#' mf_map(m_borders,
#'   col = 1:4, lwd = c(8, 6, 4, 2),
#'   add = TRUE
#' )
#' # Plot labels
#' mf_label(m_borders, "id",
#'   overlap = FALSE,
#'   lines = FALSE, halo = TRUE,
#'   col = 1:4
#' )
#' @export
mf_get_borders <- function(x, id) {
  if (get_geom_type(x) != "POLYGON") {
    stop(paste0('"x" should be a POLYGON sf object'), call. = FALSE)
  }

  if (sf::st_is_longlat(x)) {
    er <- paste0("This feature only works with projected layers.\n",
                 "It seems that you are using an unprojected geographic ",
                 "layer (using longitude and latitude).\n",
                 "You can use crssuggest::suggest_crs(x) to find a candidate CRS then ",
                 "sf::st_tranform(x, crs) to transform the layer.")
    stop(er, call. = FALSE)

  }

  if (missing(id)) {
    id <- names(x)[1]
  }
  oproj <- sf::st_crs(x)
  sf::st_geometry(x) <- sf::st_buffer(x = sf::st_geometry(x), 1, nQuadSegs = 5)
  lx <- sf::st_cast(x, "MULTILINESTRING")

  l <- sf::st_intersects(x, x, sparse = FALSE)
  colnames(l) <- x[[id]]
  rownames(l) <- x[[id]]
  l <- lower.tri(l) * l

  gna <- function(x) {
    y <- x[x == 1]
    if (length(y) > 0) {
      names(y)
    } else {
      NA
    }
  }
  myl <- as.list(apply(l, 1, gna))
  myl <- myl[!is.na(myl)]
  long <- sum(sapply(myl, length))
  df <- data.frame(id = rep(NA, long), id1 = rep(NA, long), id2 = rep(NA, long))

  lgeo <- vector(mode = "list", length = long)
  lgeo2 <- vector(mode = "list", length = long)
  ind <- 1
  for (i in 1:length(myl)) {
    id1 <- names(myl[i])
    li <- lx[lx[[id]] == id1, ]
    for (j in 1:length(myl[[i]])) {
      id2 <- myl[[i]][[j]]
      po <- x[x[[id]] == id2, ]
      Inter <- sf::st_intersection(sf::st_geometry(li), sf::st_geometry(po))
      df[ind, ] <- c(paste0(id1, "_", id2), id1, id2)
      lgeo[[ind]] <- Inter[[1]]
      ind <- ind + 1
    }
  }

  df <- sf::st_sf(df, geometry = sf::st_sfc(lgeo))
  df <- sf::st_cast(x = df, to = "MULTILINESTRING")
  df2 <- df[, c(1, 3, 2)]
  names(df2) <- c("id", "id1", "id2", "geometry")
  df2$id <- paste(df2$id1, df2$id2, sep = "_")

  borderlines <- rbind(df, df2)
  row.names(borderlines) <- borderlines$id
  borderlines <- sf::st_set_crs(borderlines, oproj)
  return(borderlines)
}
