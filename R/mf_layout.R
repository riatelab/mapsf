#' @title Plot a map layout
#' @description Plot a map layout (title, credits, scalebar,
#' north arrow, frame).
#'
#' This function uses \code{\link{mf_title}}, \code{\link{mf_credits}},
#' \code{\link{mf_scale}} and \code{\link{mf_scale}} with default values.
#'
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
    pux <- par("usr")
    rect(pux[1], pux[3], pux[2], pux[4], border = fg, col = NA)
  }
}
