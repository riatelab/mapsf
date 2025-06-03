#' @title Plot a frame
#' @description Plot a frame around an existing map.
#' @param extent type of frame, either 'map' or 'figure'
#' @param col line color
#' @param lwd line width
#' @param lty line type
#' @param ... other arguments from \code{\link[graphics:box]{box}}
#'
#' @return No return value, a frame is displayed.
#' @export
#'
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_title()
#' mf_frame(extent = "map")
#' mf_map(mtq)
#' mf_title()
#' mf_frame(extent = "figure")
mf_frame <- function(extent = "map", col, lwd = 1.5, lty = 1, ...) {
  test_cur_plot()
  if (missing(col)) {
    col <- getOption("mapsf.highlight")
  }

  if (extent == "map") {
    inner <- as.numeric(!getOption("mapsf.title_inner"))
    mar <- getOption("mapsf.mar")
    line <- getOption("mapsf.title_line")
    top <- -line * inner
    mar <- mar + c(0, 0, ifelse(top > 0, 0, top), 0)
    mar[mar < 0] <- 0
    op <- par(mar = mar, no.readonly = TRUE)
    on.exit(par(op))
    box(which = "plot", col = col, lwd = lwd, lty = lty, ...)
  }
  if (extent == "figure") {
    box(which = "figure", col = col, lwd = lwd, lty = lty, ...)
  }
  return(invisible(NULL))
}
