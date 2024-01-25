#' @title Plot an sf object
#' @description Plot an sf object. This is mostly a wrapper around
#' \code{plot(st_geometry(x), ...)}.
#' @param x	object of class \code{sf}, \code{sfc} or \code{sfg}
#' @eval my_params(c(
#' 'col',
#' 'border',
#' 'lwd', 'pch',
#' 'add'))
#' @param cex point size
#' @param bg background color
#' @param lty line or border type
#' @param ... further parameters from \link{plot} for sfc objects
#' @importFrom sf st_geometry
#' @keywords internal
#' @return x is (invisibly) returned.
#' @export
#' @examples
#' library(sf)
#' mtq <- mf_get_mtq()
#' mf_map(mtq, type = "base")
#' mf_map(mtq, type = "base", col = "blue")
mf_base <- function(x,
                    col = "grey80",
                    border = "grey20",
                    bg = "white",
                    cex = 1,
                    pch = 20,
                    lwd = .7,
                    lty = 1,
                    add = FALSE,
                    ...) {
  # margins mgmt
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))

  if (add == FALSE) {
    mf_init(x)
    add <- TRUE
  }

  xtype <- get_geom_type(x)
  if (xtype != "POLYGON" && missing(col)) {
    col <- "grey20"
  }

  plot(st_geometry(x),
    col = col, border = border,
    lwd = lwd, add = add, pch = pch,
    bg = bg, lty = lty, cex = cex,
    ...
  )

  return(invisible(x))
}
