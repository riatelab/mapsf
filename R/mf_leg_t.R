#' Plot a legend for a typology map
#' @description
#' Deprecated.
#'
#' This function plots a legend for a typology map.
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
#' @param border color of the boxes' borders
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
#' mf_legend_t(val = c("type A", "type B"), pal = c("navy", "tomato"))
#' }
mf_legend_t <- function(pos = "topright",
                        val,
                        pal,
                        title = "Legend Title",
                        title_cex = .8,
                        val_cex = .6,
                        col_na = "white",
                        no_data = FALSE,
                        no_data_txt = "No Data",
                        frame = FALSE,
                        border,
                        bg,
                        fg,
                        cex = 1) {
  .Deprecated(
    new = "maplegend::leg()",
    package = "maplegend",
    msg = paste0(
      "'mf_legend_p()' is deprecated. ",
      "Use 'maplegend::leg(type = 'typo', ...)' instead."
    ),
    old = "mf_legend()"
  )
  test_cur_plot()
  args <- as.list(match.call())
  args <- args[-1]
  args$type <- "typo"
  if (missing(bg)) args$bg <- getOption("mapsf.bg")
  if (missing(fg)) args$fg <- getOption("mapsf.fg")
  mf_call_leg(args)

  return(invisible(NULL))
}
