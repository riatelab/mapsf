#' @title Plot a map layout
#' @description Plot a map layout (title, credits, scalebar,
#' north arrow, frame).
#' @name mf_layout
#' @param title title of the map
#' @param credits credits
#' @param scale display a scale bar
#' @param arrow display an arrow
#' @param frame display a frame
#' @export
#' @return No return value, a map layout is displayed.
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_layout()
mf_layout <- function(title = "Map Title",
                      credits = "Authors & Sources",
                      scale = TRUE, arrow = TRUE, frame = FALSE) {
  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  on.exit(par(op))
  fg <- .gmapsf$args$fg

  if (title != "") {
    mf_title(txt = title)
  }

  if (credits != "") {
    mf_credits(txt = credits, pos = "bottomleft")
  }
  if (arrow) {
    mf_arrow()
  }
  if (scale) {
    mf_scale()
  }
  if (frame) {
    box(col = fg)
  }
}
