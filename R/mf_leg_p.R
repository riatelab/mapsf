#' Plot a legend for a proportional symbols map
#' @description
#' Deprecated.
#'
#' This function plots a legend for proportional symbols.
#'
#' @param symbol type of symbols, 'circle' or 'square'
#' @param inches size of the biggest symbol (radius for circles, half width
#' for squares) in inches
#' @param pos position of the legend, one of "topleft", "top",
#' "topright", "right", "bottomright", "bottom", "bottomleft",
#' "left", "interactive" or a vector of two coordinates in map units
#' (c(x, y)).
#' @param val vector of values (at least min and max).
#' @param title title of the legend
#' @param title_cex size of the legend title
#' @param val_cex size of the values in the legend
#' @param val_rnd number of decimal places of the values in
#' the legend.
#' @param frame whether to add a frame to the legend (TRUE) or not (FALSE)
#' @param border color of the symbols borders
#' @param cex size of the legend; 2 means two times bigger
#' @param lwd width of the symbols borders
#' @param col color of the symbols
#' @param bg background of the legend
#' @param fg foreground of the legend
#' @param self_adjust if TRUE values are self-adjusted to keep min, max and
#' intermediate rounded values
#' @keywords internal
#' @export
#' @return No return value, a legend is displayed.
#' @import graphics
#' @examples
#' \dontrun{
#' plot.new()
#' plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
#' mf_legend_p(val = c(1, 20, 100), col = "red", inches = .3)
#' }
mf_legend_p <- function(pos = "left",
                        val,
                        col = "tomato4",
                        inches,
                        symbol = "circle",
                        border,
                        lwd = .7,
                        title = "Legend Title",
                        title_cex = .8,
                        val_cex = .6,
                        val_rnd = 0,
                        frame = FALSE,
                        bg,
                        fg,
                        cex = 1,
                        self_adjust = FALSE) {
  message(
    msg = paste0(
      "'mf_legend_p()' is deprecated. ",
      "Use 'mf_legend(type = 'prop', ...)' ",
      "instead."
    )
  )
  args <- as.list(match.call())
  args <- args[-1]
  args$type <- "prop"
  do.call(mf_legend, clean_leg_args(args))
  return(invisible(NULL))
}
