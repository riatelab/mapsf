#' Plot a legend for a proportional lines map
#' @description
#'
#' Deprecated.
#'
#' This function plots a legend for proportional lines.
#'
#' @param pos position of the legend, one of "topleft", "top",
#' "topright", "right", "bottomright", "bottom", "bottomleft",
#' "left", "interactive" or a vector of two coordinates in map units
#' (c(x, y)).
#' @param lwd width of the largest line
#' @param val vector of values (at least min and max).
#' @param title title of the legend
#' @param title_cex size of the legend title
#' @param val_cex size of the values in the legend
#' @param val_rnd number of decimal places of the values in
#' the legend.
#' @param frame whether to add a frame to the legend (TRUE) or not (FALSE)
#' @param cex size of the legend; 2 means two times bigger
#' @param col color of the lines
#' @param bg background of the legend
#' @param fg foreground of the legend
#' @keywords internal
#' @export
#' @return No return value, a legend is displayed.
#' @import graphics
#' @examples
#' \dontrun{
#' plot.new()
#' plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
#' mf_legend_pl(lwd = 20, val = c(5, 10, 50, 100))
#' }
mf_legend_pl <- function(pos = "left",
                         val,
                         lwd,
                         col = "tomato4",
                         title = "Legend Title",
                         title_cex = .8,
                         val_cex = .6,
                         val_rnd = 0,
                         frame = FALSE,
                         bg,
                         fg,
                         cex = 1) {
  message(
    msg = paste0(
      "'mf_legend_pl()' is deprecated. ",
      "Use 'mf_legend(type = 'prop_line', ...)' ",
      "instead."
    )
  )
  args <- as.list(match.call())
  args <- args[-1]
  args$type <- "prop_line"
  do.call(mf_legend, clean_leg_args(args))
  return(invisible(NULL))
}
