#' @title Plot a legend for a choropleth map
#' @description
#' Deprecated.
#'
#' This function plots a legend for a choropleth map.
#'
#' @param pal a set of colors.
#' @param col_na color for missing values
#' @param pos position of the legend, one of "topleft", "top",
#' "topright", "right", "bottomright", "bottom", "bottomleft",
#' "left", "interactive" or a vector of two coordinates in map units
#' (c(x, y))
#' @param val break labels
#' @param title title of the legend
#' @param title_cex size of the legend title
#' @param val_cex size of the values in the legend
#' @param val_rnd number of decimal places of the values in
#' the legend.
#' @param no_data if TRUE a "missing value" box is plotted
#' @param no_data_txt label for missing values
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
#' mf_legend_c(val = c(1, 2, 3, 4), pal = c("red1", "red3", "red4"))
#' }
mf_legend_c <- function(pos = "topleft",
                        val,
                        pal,
                        title = "Legend Title",
                        title_cex = .8,
                        val_cex = .6,
                        val_rnd = 2,
                        col_na = "white",
                        no_data = FALSE,
                        no_data_txt = "No Data",
                        frame = FALSE,
                        border,
                        bg,
                        fg,
                        cex = 1) {
  .Deprecated(
    new = "mapsf::mf_legend()",
    package = "mapsf",
    msg = paste0(
      "'mf_legend_c()' is deprecated. ",
      "Use 'mf_legend(type = 'choro', ...)' ",
      "instead."
    ),
    old = "mf_legend_c()"
  )
  args <- as.list(match.call())
  args <- args[-1]
  args$type <- "choro"
  do.call(mf_legend, clean_leg_args(args))
  return(invisible(NULL))
}
