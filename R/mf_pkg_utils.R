#
# import stuffs
# @import graphics
# @import stats
#' @import sf
#' @importFrom maplegend leg leg_comp leg_draw
#' @importFrom grDevices colorRampPalette
#' @importFrom stats aggregate median na.omit quantile runif sd




#' @importFrom utils globalVariables
.gmapsf <- new.env(parent = emptyenv())
globalVariables(".gmapsf", package = "mapsf", add = FALSE)
.gmapsf$inset <- FALSE
.gmapsf$positions <- c(
  "bottomleft", "left", "topleft", "top", "bottom",
  "bottomright", "right", "topright",
  "interactive"
)



.gmapsf$themes <- list(
  default = list(
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


.onLoad <- function(libname, pkgname) {
  load_default_theme()
}



load_default_theme <- function() {
  theme <- .gmapsf$themes$default
  options(
    mapsf.bg = theme$bg,
    mapsf.fg = theme$fg,
    mapsf.mar = theme$mar,
    mapsf.tab = theme$tab,
    mapsf.pos = theme$pos,
    mapsf.inner = theme$inner,
    mapsf.line = theme$line,
    mapsf.cex = theme$cex,
    mapsf.font = theme$font
  )
}



#' @importFrom grDevices dev.list
test_cur_plot <- function() {
  if (is.null(dev.list())) {
    stop("You can only use this feature on an existing plot.", call. = FALSE)
  }
}


interleg <- function(txt = c("legend", "Legend")) {
  if (interactive()) {
    message(paste0("Click on the map to choose the ", txt[1], " position."))
    x <- unlist(locator(1))
    message(paste0(txt[2], " coordinates:\nc(", x[[1]], ", ", x[[2]], ")"))
    return(x)
  } else {
    stop('You cannot use "interactive" in a non-interactive R session.',
      call. = FALSE
    )
  }
}


# shadow around the labels
#' @name shadowtext
#' @title shadowtext
#' @description shadowtext
#' @param x lon
#' @param y lat
#' @param labels labels
#' @param col col
#' @param bg bg
#' @param theta number of iteration
#' @param r radius
#' @param ... other txt params
#' @noRd
shadowtext <- function(x, y = NULL, labels, col = "white", bg = "black",
                       theta = seq(0, 2 * pi, length.out = 50), r = 0.1, ...) {
  xo <- r * strwidth("A")
  yo <- r * strheight("A")
  for (i in theta) {
    text(x + cos(i) * xo, y + sin(i) * yo, labels, col = bg, ...)
  }
  text(x, y, labels, col = col, ...)
}
