#' @title Defunct - Export a map
#' @name mf_export
#' @description
#' This function is defunct. Please use [mf_png] or [mf_svg] instead.
#' @export
#' @keywords internal
#' @return No return value.
#' @examples
#' mtq <- mf_get_mtq()
#' (filename <- tempfile(fileext = ".png"))
#' mf_png(mtq, filename = filename)
#' mf_map(mtq)
#' mf_title()
#' dev.off()
mf_export <- function(...) {
  .Defunct(
    msg = "'mf_export()' is defunct.\nUse 'mf_png()' or `mf_svg()` instead."
  )
}
