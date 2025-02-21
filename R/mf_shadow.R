#' @title Plot a shadow
#' @name mf_shadow
#' @description Plot the shadow of a polygon layer.
#' @param x an sf or sfc polygon object
#' @param col shadow color
#' @param cex shadow extent
#' @eval my_params("add")
#' @export
#' @importFrom sf st_geometry
#' @return x is (invisibly) returned.
#' @examples
#' mtq <- mf_get_mtq()
#' mf_shadow(mtq)
#' mf_map(mtq, add = TRUE)
mf_shadow <- function(x, col, cex = 1, add = FALSE) {
  if (add == FALSE) {
    mf_init(x)
  }
  col <- go(col, "highlight", "grey50")
  d <- cex * strwidth("M", units = "user", cex = 1) / 3
  z <- st_geometry(x) + c(d, -d)

  mf_map(
    x = z, type = "base", col = col,
    border = col, lwd = .5, add = TRUE
  )
  return(invisible(x))
}
