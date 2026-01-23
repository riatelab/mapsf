#' @title Plot a shadow
#' @name mf_shadow
#' @description Plot the shadow of a polygon layer.
#' @param x an sf or sfc polygon object
#' @param col shadow color
#' @param cex shadow extent
#' @param expandBB fractional values to expand the bounding box with, in each
#' direction (bottom, left, top, right)
#' @eval my_params(c(
#' "add",
#' "extent",
#' "bg")
#' )
#' @export
#' @importFrom sf st_geometry
#' @return x is (invisibly) returned.
#' @examples
#' mtq <- mf_get_mtq()
#' mf_shadow(mtq)
#' mf_map(mtq, add = TRUE)
mf_shadow <- function(x, col, cex = 1, add = FALSE, extent = x, bg,
                      expandBB = rep(.04, 4)) {
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))
  bgc <- go(bg, "background")

  if (add == FALSE) {
    mf_init(x, expandBB = expandBB, extent = extent, bgc = bgc)
  }
  col <- go(col, "highlight", "grey50")
  xyi <- xyinch(1)
  ratio <- xyi[2] / xyi[1]
  d_x <- cex * strwidth("M", units = "user", cex = 1) / 3
  d_y <- d_x * ratio
  z <- st_geometry(x) + c(d_x, -d_y)
  mf_map(
    x = z, type = "base", col = col,
    border = col, lwd = .5, add = TRUE
  )
  return(invisible(x))
}
