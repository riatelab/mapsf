#' @title Plot a legend for a graduated lines map
#' @description
#' Deprecated.
#'
#' This function plots a legend for graduated lines maps.
#'
#' @param pos position of the legend, one of "topleft", "top",
#' "topright", "right", "bottomright", "bottom", "bottomleft",
#' "left", "interactive" or a vector of two coordinates in map units
#' (c(x, y))
#' @param lwd lines widths
#' @param val break labels
#' @param col lines color
#' @param title title of the legend
#' @param title_cex size of the legend title
#' @param val_cex size of the values in the legend
#' @param val_rnd number of decimal places of the values in
#' the legend.
#' @param frame whether to add a frame to the legend (TRUE) or not (FALSE)
#' @param cex size of the legend; 2 means two times bigger
#' @param bg background of the legend
#' @param fg foreground of the legend
#' @keywords internal
#' @export
#' @import graphics
#' @return No return value, a legend is displayed.
#' @examples
#' \dontrun{
#' plot.new()
#' plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
#' mf_legend_gl(lwd = c(0.2, 2, 4, 5, 10), val = c(1, 2, 3, 4, 10.2, 15.2))
#' }
mf_legend_gl <- function(pos = "topleft", val,
                         col = "tomato4",
                         lwd,
                         title = "Legend Title",
                         title_cex = .8,
                         val_cex = .6,
                         val_rnd = 2,
                         frame = FALSE,
                         bg,
                         fg,
                         cex = 1) {
  .Deprecated(new = "maplegend::leg()",
              package = "maplegend",
              msg = paste0("'mf_legend_gl()' is deprecated. ",
                           "Use 'maplegend::leg(type = 'grad_line', ...)' ",
                           "instead."),
              old = "mf_legend()")

  test_cur_plot()
  args <- as.list(match.call())
  args <- args[-1]
  args$type <- "grad_line"
  if (missing(bg)) args$bg <- getOption("mapsf.bg")
  if (missing(fg)) args$fg <- getOption("mapsf.fg")
  mf_call_leg(args)


  return(invisible(NULL))
}
