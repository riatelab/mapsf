#' @title Plot a legend
#' @description
#' This function is deprecated. Use maplegend::leg() instead.
#'
#' mf_legend is a wrapper for maplegend::leg().
#'
#' Plot all types of legend.
#' @md
#' @param type type of legend; one of "prop", "choro", "typo", "symb",
#' "prop_line", "grad_line"
#' @eval my_params(c("pos", "inches", "border", "symbol", "title",
#' "title_cex", "val_cex", "val_rnd", "frame", "no_data", "no_data_txt",
#' "bg", "fg", "cex", "pal"))
#' @param val a vector of values
#' @param col a color
#' @param lwd line width(s)
#' @param pt_cex cex of the symbols
#' @param pt_pch pch of the symbols (0:25)
#' @param col_na color for missing values
#' @param pt_cex_na cex of the symbols for missing values
#' @param pt_pch_na pch of the symbols for missing values
#' @param ... other arguments passed to maplegend::leg()
#' @return No return value, a legend is displayed.
#' @export
#'
#' @examples
#' \dontrun{
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_legend(type = "prop", pos = "topright", val = c(1, 5, 10), inches = .3)
#' mf_legend(
#'   type = "choro", pos = "bottomright", val = c(10, 20, 30, 40, 50),
#'   pal = hcl.colors(4, "Reds 2")
#' )
#' mf_legend(
#'   type = "typo", pos = "topleft", val = c("A", "B", "C", "D"),
#'   pal = hcl.colors(4, "Dynamic")
#' )
#' mf_legend(
#'   type = "symb", pos = "bottomleft", val = c("A", "B", "C"),
#'   pt_pch = 21:23, pt_cex = c(1, 2, 2),
#'   pal = hcl.colors(3, "Dynamic")
#' )
#' mf_legend(
#'   type = "grad_line", pos = "top", val = c(1, 2, 3, 4, 10, 15),
#'   lwd = c(0.2, 2, 4, 5, 10)
#' )
#' mf_legend(type = "prop_line", pos = "bottom", lwd = 20, val = c(5, 50, 100))
#' }
mf_legend <- function(type, pos, val, pal,
                      col,
                      inches,
                      lwd, border, symbol,
                      pt_pch, pt_cex,
                      title, title_cex, val_cex, val_rnd,
                      col_na, pt_cex_na, pt_pch_na,
                      no_data, no_data_txt,
                      frame, bg, fg, cex, ...) {
  .Deprecated(
    new = "maplegend::leg()",
    package = "maplegend",
    msg = paste0(
      "'mf_legend()' is deprecated. ",
      "Use 'maplegend::leg(type = '",
      type,
      "', ...)' instead."
    ),
    old = "mf_legend()"
  )
  test_cur_plot()
  args <- as.list(match.call())
  args <- args[-1]
  if (missing(bg)) args$bg <- getOption("mapsf.bg")
  if (missing(fg)) args$fg <- getOption("mapsf.fg")

  mf_call_leg(args)



  return(invisible(NULL))
}



mf_call_leg <- function(args) {
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))
  names_args <- names(args)
  names(args)[which(names_args == "cex")] <- "size"
  names(args)[which(names_args == "pt_pch")] <- "pch"
  names(args)[which(names_args == "pt_cex")] <- "cex"
  names(args)[which(names_args == "pt_cex_na")] <- "cex_na"
  names(args)[which(names_args == "pt_pch_na")] <- "pch_na"
  do.call(what = get("leg"), args, envir = parent.frame())
}
