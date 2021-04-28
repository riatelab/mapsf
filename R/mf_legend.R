#' @title Plot a legend
#' @description Plot all types of legend.
#' The "type" argument defines the legend type:
#' * **prop**, for proportional symbols maps, see \link{mf_legend_p}
#' for arguments, default values and details;
#' * **choro**, for choropleth maps, see \link{mf_legend_c}
#' for arguments, default values and details;
#' * **typo**, for typology maps, see \link{mf_legend_t} for arguments,
#' default values and details;
#' * **symb** for symbols maps, see \link{mf_legend_s}
#' for arguments, default values and details;
#' * **prop_line**, for proportional lines maps, see \link{mf_legend_pl}
#' for arguments, default values and details;
#' * **grad_line** for graduated lines maps, see \link{mf_legend_gl},
#' for arguments, default values and details.
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
#' @return No return value, a legend is displayed.
#' @export
#'
#' @examples
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
mf_legend <- function(type, pos, val, pal,
                      col,
                      inches,
                      lwd, border, symbol,
                      pt_pch, pt_cex,
                      title, title_cex, val_cex, val_rnd,
                      col_na, pt_cex_na, pt_pch_na,
                      no_data, no_data_txt,
                      frame, bg, fg, cex) {
  args <- as.list(match.call())
  args <- args[names(args) != "type"]

  args <- args[-1]

  op <- par(mar = .gmapsf$args$mar, no.readonly = TRUE)
  on.exit(par(op))

  if (missing(bg)) args$bg <- .gmapsf$args$bg
  if (missing(fg)) args$fg <- .gmapsf$args$fg
  switch(type,
    prop = do.call(what = mf_legend_p, args),
    choro = do.call(what = mf_legend_c, args),
    typo = do.call(what = mf_legend_t, args),
    symb = do.call(what = mf_legend_s, args),
    prop_line = do.call(what = mf_legend_pl, args),
    grad_line = do.call(what = mf_legend_gl, args)
  )
  return(invisible(NULL))
}
