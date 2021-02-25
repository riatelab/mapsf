#' @title Plot an sf object
#' @description Plot an sf object. This is mostly a wrapper around
#' \code{plot(st_geometry(x), ...)}.
#' @eval my_params(c(
#' 'xfull',
#' 'col',
#' 'border',
#' 'lwd',
#' 'add' ))
#' @param ... further parameters from \link{plot} for sfc objects
#'
#' @importFrom methods is
#' @importFrom sf st_geometry
#' @keywords internal
#' @return No return value, a map is displayed.
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
                    add = FALSE,
                    ...) {
  # margins mgmt
  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  on.exit(par(op))
  bg <- .gmapsf$args$bg


  plot(st_geometry(x),
    col = col, border = border,
    lwd = lwd, add = add, bg = bg,
    asp = 1, ...
  )
  if (add == FALSE) {
    pu <- par("usr")
    rect(pu[1], pu[3], pu[2], pu[4], border = bg, col = NA)
  }
  return(invisible(NULL))
}
