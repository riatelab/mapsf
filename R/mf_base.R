#' @title Plot an sf object
#' @description Plot an sf object. This is mostly a wrapper around
#' \code{plot(st_geometry(x), ...)}.
#' @param x	object of class \code{sf}, \code{sfc} or \code{sfg}
#' @eval my_params(c(
#' 'col',
#' 'border',
#' 'lwd',
#' 'pch',
#' 'alpha',
#' 'add'))
#' @param cex point size
#' @param lty line or border type
#' @param ... ignored
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
                    col,
                    border,
                    alpha = NULL,
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

  if (xtype == "LINE") {
    col <- go(col, "highlight", "grey20")
    if (!is.null(alpha)) {
      col <- get_hex_pal(col, alpha)
    }
    plot(
      st_geometry(x),
      col = col, lwd = lwd, lty = lty,
      add = TRUE
    )
  }

  if (xtype == "POLYGON") {
    col <- go(col, "foreground", "grey80")
    border <- go(border, "highlight", "grey20")
    if (!is.null(alpha)) {
      col <- get_hex_pal(col, alpha)
    }
    plot(
      st_geometry(x),
      col = col, border = border, lwd = lwd, lty = lty,
      add = TRUE
    )
  }

  if (xtype == "POINT") {
    col <- go(col, "highlight", "grey20")
    if (!is.null(alpha)) {
      col <- get_hex_pal(col, alpha)
    }
    if (pch %in% 21:25) {
      if (missing(border)) {
        border <- go(border, "foreground", "grey80")
      }
      mycolspt <- border
    } else {
      mycolspt <- col
    }
    mycolsptbg <- col
    plot(
      st_geometry(x),
      col = mycolspt, bg = mycolsptbg, cex = cex, pch = pch,
      lwd = lwd, add = TRUE
    )
  }

  return(invisible(x))
}
