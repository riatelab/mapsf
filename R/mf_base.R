#' @title Plot an sf object
#' @description Plot an sf object. This is mostly a wrapper around
#' \code{plot(st_geometry(x), ...)}.
#' @param x	object of class \code{sf}, \code{sfc} or \code{sfg}
#' @eval my_params(c(
#' 'col',
#' 'border',
#' 'lwd', 'pch',
#' 'add'))
#' @param ... further parameters from \link{plot} for sfc objects
#'
#' @importFrom methods is
#' @importFrom sf st_geometry
#' @keywords internal
#' @return x is (invisibly) returned.
#' @export
#' @examples
#' library(sf)
#' mtq <- mf_get_mtq()
#' mf_base(mtq)
#' mf_base(mtq, col = "blue")
mf_base <- function(x,
                    col = "grey80",
                    border = "grey20",
                    lwd = .7,
                    pch = 20,
                    add = FALSE,
                    ...) {
  # margins mgmt
  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
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
    lwd = lwd, add = add, pch = pch, ...
  )


  return(invisible(x))
}
