#' Plot a legend for a symbols map
#' @description
#'
#' Deprecated.
#'
#'
#' This function can plot a legend for a symbols maps.
#'
#' @param pal a set of colors
#' @param col_na color for missing values
#' @param pos position of the legend, one of "topleft", "top",
#' "topright", "right", "bottomright", "bottom", "bottomleft",
#' "left", "interactive" or a vector of two coordinates in map units
#' (c(x, y)).
#' @param val vector of categories.
#' @param title title of the legend
#' @param title_cex size of the legend title
#' @param val_cex size of the values in the legend
#' @param no_data if TRUE a "missing value" box is plotted
#' @param no_data_txt label for missing values.
#' @param frame whether to add a frame to the legend (TRUE) or not (FALSE)
#' @param border type = "prop": color of the symbols borders
#' @param cex size of the legend; 2 means two times bigger
#' @param bg background of the legend
#' @param fg foreground of the legend
#' @param pt_cex cex of the symbols
#' @param pt_pch pch of the symbols (0:25)
#' @param pt_cex_na cex of the symbols for missing values
#' @param pt_pch_na pch of the symbols for missing values
#' @param lwd width of the symbols borders
#' @keywords internal
#' @export
#' @import graphics
#' @return No return value, a legend is displayed.
#' @examples
#' plot.new()
#' plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
#' mf_legend_s(
#'   val = c("Type C", "Type D"), pal = c("cyan", "plum"),
#'   pt_pch = c(21, 23), pt_cex = c(1, 2)
#' )
mf_legend_s <- function(pos = "right",
                        val,
                        pal,
                        pt_pch,
                        pt_cex,
                        border,
                        lwd = .7,
                        title = "Legend title",
                        title_cex = .8,
                        val_cex = .6,
                        pt_cex_na = 1,
                        pt_pch_na = 4,
                        col_na = "white",
                        no_data = FALSE,
                        no_data_txt = "No Data",
                        frame = FALSE,
                        bg,
                        fg,
                        cex = 1) {
  message(
    msg = paste0(
      "'mf_legend_s()' is deprecated. ",
      "Use 'mf_legend(type = 'symb', ...)' ",
      "instead."
    )
  )
  args <- as.list(match.call())
  args <- args[-1]
  args$type <- "symb"
  do.call(mf_legend, clean_leg_args(args))
  return(invisible(NULL))
}


clean_leg_args <- function(args) {
  names_args <- names(args)
  names(args)[which(names_args == "cex")] <- "size"
  args
}
