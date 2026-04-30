#' @title Get the optimal position of a legend
#'
#' @description Find the optimal for one or two legends. The optimal position is
#' a position that minimizes overlap between a spatial object and a legend.
#' @eval my_params(c("x"))
#' @param n number of positions to get (1 or 2)
#'
#' @return A vector of position is returned
#' @keywords internal
#' @export
#'
#' @examples
#' mtq <- mf_get_mtq()
#' mf_get_leg_pos(mtq)
mf_get_leg_pos <- function(x, n = 1) {
  if (is.null(dev.list())) {
    bb <- st_bbox(x)
  } else {
    p <- par("usr")
    bb <- st_bbox(
      c(
        xmin = p[1], ymin = p[3],
        xmax = p[2], ymax = p[4]
      ),
      crs = st_crs(x)
    )
  }
  g <- st_make_grid(x = st_as_sfc(bb), n = c(3, 3), crs = st_crs(x))
  g <- g[c(2, 4, 5, 6, 7, 8, 9)]
  y <- st_union(st_convex_hull(st_geometry(x)))
  z <- st_intersects(g, y)
  ind <- which(unlist(lapply(z, length)) == 0)
  if (n == 1) {
    if (length(ind) > 0) {
      pos <- max(ind)
    } else {
      pos <- which.min(st_area(st_intersection(g, y)))
    }
  }
  if (n == 2) {
    pos <- c(NA, NA)
    if (length(ind) > 1) {
      pos <- sort(ind, decreasing = TRUE)[1:2]
    } else {
      if (length(ind) == 1) {
        ii <- rep(NA, 7)
        names(ii) <- 1:7
        ii[ind] <- 0
        couv <- st_area(st_intersection(g, y))
        ii[is.na(ii)] <- couv
        pos <- as.numeric(names(sort(ii)[1:2]))
      } else {
        couv <- st_area(st_intersection(g, y))
        names(couv) <- 1:7
        pos <- as.numeric(names(sort(couv)[1:2]))
      }
    }
  }
  tpos <- c(
    "bottom",
    "left", "center", "right",
    "topleft", "top", "topright"
  )
  return(tpos[pos])
}
