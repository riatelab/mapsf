#' @title Set a theme
#' @description This function set a map theme.
#' The parameters set by this function are the figure margins, background and
#' foreground colors and some \link{mf_title} options.
#' @param x name of a map theme. One of "default", "brutal", "ink",
#' "dark", "agolalight", "candy", "darkula", "iceberg", "green", "nevermind",
#' "jsk", "barcelona". If x is used other parameters are ignored.
#' @param bg background color
#' @param fg foreground color
#' @param mar margins
#' @param pos position, one of 'left', 'center', 'right'
#' @param tab if TRUE the title is displayed as a 'tab'
#' @param cex cex of the title
#' @param font font of the title
#' @param line number of lines used for the title
#' @param inner if TRUE the title is displayed inside the plot area.
#' @details
#' It is also possible to set a custom theme using a list of arguments
#' (see Examples).
#' Use \code{mf_theme('default')} to reset theme settings.
#' \code{mf_theme()} returns the current theme settings.
#' @return The (invisible) list of theme parameters is returned.
#' @export
#' @examples
#' mtq <- mf_get_mtq()
#'
#' # built-in theme
#' mf_theme("green")
#' mf_map(mtq)
#' mf_title()
#'
#' # theme from arguments
#' mf_theme(
#'   bg = "darkslategrey", fg = "cornsilk3", mar = c(2, 2, 4, 2),
#'   tab = FALSE, pos = "center", inner = FALSE,
#'   line = 2, cex = 2, font = 4
#' )
#' mf_map(mtq)
#' mf_layout()
#'
#' # theme from list
#' custom <- list(
#'   name = "custom",
#'   bg = "green",
#'   fg = "red",
#'   mar = c(2, 2, 2, 2),
#'   tab = TRUE,
#'   pos = "center",
#'   inner = TRUE,
#'   line = 2,
#'   cex = 1.5,
#'   font = 3
#' )
#' mf_theme(custom)
#' mf_map(mtq)
#' mf_title()
#'
#' (mf_theme("default"))
mf_theme <- function(x = "default", bg, fg, mar, tab, pos, inner, line, cex,
                     font) {
  themes <- list(
    default = list(
      name = "default",
      bg = "#f7f7f7",
      fg = "#333333",
      mar = c(.5, .5, 1.7, .5),
      tab = TRUE,
      pos = "left",
      inner = FALSE,
      line = 1.2,
      cex = 1,
      font = 1
    ),
    brutal = list(
      name = "brutal",
      bg = "#FFFFFF",
      fg = "#3b4252",
      mar = c(5.1, 4.1, 4.1, 2.1),
      tab = TRUE,
      pos = "left",
      inner = FALSE,
      line = 2,
      cex = 1.5,
      font = 2
    ),
    ink = list(
      name = "ink",
      bg = "#FFDEAD",
      fg = "#0000FF",
      mar = c(0, 0, 1.2, 0),
      tab = FALSE,
      pos = "left",
      inner = FALSE,
      line = 1.2,
      cex = .9,
      font = 2
    ),
    dark = list(
      name = "dark",
      bg = "#2E3947",
      fg = "#7E848C",
      mar = c(0.5, 0.5, 2, 0.5),
      tab = FALSE,
      pos = "left",
      inner = FALSE,
      line = 1.5,
      cex = 1,
      font = 1
    ),
    agolalight = list(
      name = "agolalight",
      bg = "#EDF4F5",
      fg = "#82888A",
      mar = c(0, 0, 2, 0),
      tab = FALSE,
      pos = "left",
      inner = FALSE,
      line = 2,
      cex = 1.5,
      font = 3
    ),
    candy = list(
      name = "candy",
      bg = "#FDFCFE",
      fg = "#6B1767",
      mar = c(0, 0, 2, 0),
      tab = FALSE,
      pos = "center",
      inner = FALSE,
      line = 2,
      cex = 1.5,
      font = 2
    ),
    darkula = list(
      name = "darkula",
      bg = "#232525",
      fg = "#A9B7C6",
      mar = c(0.5, 0.5, 0.5, 0.5),
      tab = TRUE,
      pos = "right",
      inner = TRUE,
      line = 1.5,
      cex = 1,
      font = 4
    ),
    iceberg = list(
      name = "iceberg",
      bg = "#0B0E0E",
      fg = "#BDD6DB",
      mar = c(0.5, 0.5, 0.5, 0.5),
      tab = TRUE,
      pos = "right",
      inner = TRUE,
      line = 1.5,
      cex = 1,
      font = 4
    ),
    green = list(
      name = "green",
      bg = "#1B1D16",
      fg = "#D7FF68",
      mar = c(0.5, 0.5, 2, 0.5),
      tab = FALSE,
      pos = "center",
      inner = FALSE,
      line = 1.5,
      cex = 1,
      font = 2
    ),
    nevermind = list(
      name = "nevermind",
      bg = "#4DB8DA",
      fg = "#121725",
      mar = c(2, 2, 3.5, 2),
      tab = FALSE,
      pos = "center",
      inner = FALSE,
      line = 1.5,
      cex = 1.4,
      font = 1
    ),
    jsk = list(
      name = "jsk",
      bg = "#ffdc11",
      fg = "#0c973c",
      mar = c(0, 0, 1.5, 0),
      tab = FALSE,
      pos = "left",
      inner = FALSE,
      line = 1.5,
      cex = 1,
      font = 2
    ),
    barcelona = list(
      name = "barcelona",
      bg = "#160808",
      fg = "#d73e23",
      mar = c(0, 0, 1.2, 0),
      tab = TRUE,
      pos = "left",
      inner = FALSE,
      line = 1.2,
      cex = 1,
      font = 2
    )
  )


  if (missing(x)) {
    x <- .gmapsf$args
  }

  if (is.list(x)) {
    theme <- x
  } else {
    if (!x %in% names(themes)) {
      stop(
        paste0(
          "x should be one of ",
          paste0(names(themes), collapse = ", ")
        ),
        call. = FALSE
      )
    } else {
      theme <- themes[[x]]
    }
  }



  if (!missing(bg)) theme$bg <- bg
  if (!missing(fg)) theme$fg <- fg
  if (!missing(mar)) theme$mar <- mar
  if (!missing(tab)) theme$tab <- tab
  if (!missing(pos)) theme$pos <- pos
  if (!missing(inner)) theme$inner <- inner
  if (!missing(line)) theme$line <- line
  if (!missing(cex)) theme$cex <- cex
  if (!missing(font)) theme$font <- font




  .gmapsf$args <- as.list(theme)

  return(invisible(.gmapsf$args))
}
