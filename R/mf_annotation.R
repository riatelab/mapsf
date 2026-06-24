#' @title Defunct - Plot an annotation
#' @description
#' This function is defunct. Please use [mf_text] instead.
#' @export
#' @keywords internal
#' @return No return value.
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_text(
#'   x = mtq[2, ],
#'   txt = "pos = 'bottomleft'\nline = 2\nclockwise = FALSE",
#'   pos = "bottomleft",
#'   offset = 6,
#'   clockwise = FALSE,
#'   line = 2,
#'   box = FALSE
#' )
#' mf_text(
#'   x = mtq[28, ],
#'   txt = "pos = 'topright'\nline = 3\nclockwise = FALSE",
#'   pos = "topright",
#'   offset = 10,
#'   clockwise = FALSE,
#'   line = 3,
#'   halo = TRUE,
#'   align = "left"
#' )
mf_annotation <- function(...) {
  .Defunct(msg = "'mf_annotation()' is defunct.\nUse 'mf_text()' instead.")
}
