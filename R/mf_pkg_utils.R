#
# import stuffs
# @import graphics
# @import stats
#' @import sf
#' @import graphics
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

go <- function(x, opt, legacy) {
  if (missing(x)) {
    if (is.null(getOption("mapsf.legacy")) || missing(legacy)) {
      x <- getOption(paste0("mapsf.", opt))
    } else {
      x <- legacy
    }
  }
  return(x)
}

.gmapsf$themes <- list(
  base = list(
    mar          = c(0.5, 0.5, 2.25, 0.5),
    title_tab    = FALSE,
    title_pos    = "center",
    title_inner  = FALSE,
    title_line   = 1.75,
    title_cex    = 1.25,
    title_font   = 2,
    title_banner = TRUE,
    foreground   = "#B8C4CE",
    background   = "#F9F6EE",
    highlight    = "#500f27",
    pal_quali    = "Dark 3",
    pal_seq      = colorRampPalette(c("#D4E1EC", "#6A8398", "#103E65"))
  ),
  default = list(
    mar = c(.5, .5, 1.7, .5),
    title_tab = TRUE,
    title_pos = "left",
    title_inner = FALSE,
    title_line = 1.2,
    title_cex = 1,
    title_font = 1,
    title_banner = TRUE,
    foreground = "#333333",
    background = "#f7f7f7",
    highlight = "#333333",
    pal_quali = "Set 2",
    pal_seq = "Mint",
    legacy = TRUE
  ),
  brutal = list(
    background = "#FFFFFF",
    foreground = "#3b4252",
    mar = c(5.1, 4.1, 4.1, 2.1),
    title_tab = TRUE,
    title_pos = "left",
    title_inner = FALSE,
    title_line = 2,
    title_cex = 1.5,
    title_font = 2,
    highlight = "#3b4252",
    pal_quali = "Set 2",
    pal_seq = "Mint",
    legacy = TRUE
  ),
  ink = list(
    background = "#FFDEAD",
    foreground = "#0000FF",
    mar = c(0, 0, 1.2, 0),
    title_tab = FALSE,
    title_pos = "left",
    title_inner = FALSE,
    title_line = 1.2,
    title_cex = .9,
    title_font = 2,
    highlight = "#0000FF",
    pal_quali = "Set 2",
    pal_seq = "Mint",
    legacy = TRUE
  ),
  dark = list(
    background = "#2E3947",
    foreground = "#7E848C",
    mar = c(0.5, 0.5, 2, 0.5),
    title_tab = FALSE,
    title_pos = "left",
    title_inner = FALSE,
    title_line = 1.5,
    title_cex = 1,
    title_font = 1,
    highlight = "#7E848C",
    pal_quali = "Set 2",
    pal_seq = "Mint",
    legacy = TRUE
  ),
  agolalight = list(
    background = "#EDF4F5",
    foreground = "#82888A",
    mar = c(0, 0, 2, 0),
    title_tab = FALSE,
    title_pos = "left",
    title_inner = FALSE,
    title_line = 2,
    title_cex = 1.5,
    title_font = 3,
    highlight = "#82888A",
    pal_quali = "Set 2",
    pal_seq = "Mint",
    legacy = TRUE
  ),
  candy = list(
    background = "#FDFCFE",
    foreground = "#6B1767",
    mar = c(0, 0, 2, 0),
    title_tab = FALSE,
    title_pos = "center",
    title_inner = FALSE,
    title_line = 2,
    title_cex = 1.5,
    title_font = 2,
    highlight = "#6B1767",
    pal_quali = "Set 2",
    pal_seq = "Mint",
    legacy = TRUE
  ),
  darkula = list(
    background = "#232525",
    foreground = "#A9B7C6",
    mar = c(0.5, 0.5, 0.5, 0.5),
    title_tab = TRUE,
    title_pos = "right",
    title_inner = TRUE,
    title_line = 1.5,
    title_cex = 1,
    title_font = 4,
    highlight = "#A9B7C6",
    pal_quali = "Set 2",
    pal_seq = "Mint",
    legacy = TRUE
  ),
  iceberg = list(
    background = "#0B0E0E",
    foreground = "#BDD6DB",
    mar = c(0.5, 0.5, 0.5, 0.5),
    title_tab = TRUE,
    title_pos = "right",
    title_inner = TRUE,
    title_line = 1.5,
    title_cex = 1,
    title_font = 4,
    highlight = "#BDD6DB",
    pal_quali = "Set 2",
    pal_seq = "Mint",
    legacy = TRUE
  ),
  green = list(
    background = "#1B1D16",
    foreground = "#D7FF68",
    mar = c(0.5, 0.5, 2, 0.5),
    title_tab = FALSE,
    title_pos = "center",
    title_inner = FALSE,
    title_line = 1.5,
    title_cex = 1,
    title_font = 2,
    highlight = "#D7FF68",
    pal_quali = "Set 2",
    pal_seq = "Mint",
    legacy = TRUE
  ),
  nevermind = list(
    background = "#4DB8DA",
    foreground = "#121725",
    mar = c(2, 2, 3.5, 2),
    title_tab = FALSE,
    title_pos = "center",
    title_inner = FALSE,
    title_line = 1.5,
    title_cex = 1.4,
    title_font = 1,
    highlight = "#121725",
    pal_quali = "Set 2",
    pal_seq = "Mint",
    legacy = TRUE
  ),
  jsk = list(
    background = "#ffdc11",
    foreground = "#0c973c",
    mar = c(0, 0, 1.5, 0),
    title_tab = FALSE,
    title_pos = "left",
    title_inner = FALSE,
    title_line = 1.5,
    title_cex = 1,
    title_font = 2,
    highlight = "#0c973c",
    pal_quali = "Set 2",
    pal_seq = "Mint",
    legacy = TRUE
  ),
  barcelona = list(
    background = "#160808",
    foreground = "#d73e23",
    mar = c(0, 0, 1.2, 0),
    title_tab = TRUE,
    title_pos = "left",
    title_inner = FALSE,
    title_line = 1.2,
    title_cex = 1,
    title_font = 2,
    highlight = "#d73e23",
    pal_quali = "Set 2",
    pal_seq = "Mint",
    legacy = TRUE
  )
)


.onLoad <- function(libname, pkgname) {
  theme <- .gmapsf$themes$base
  options(
    mapsf.mar          = theme$mar,
    mapsf.title_tab    = theme$title_tab,
    mapsf.title_pos    = theme$title_pos,
    mapsf.title_inner  = theme$title_inner,
    mapsf.title_line   = theme$title_line,
    mapsf.title_cex    = theme$title_cex,
    mapsf.title_font   = theme$title_font,
    mapsf.title_banner = theme$title_banner,
    mapsf.foreground   = theme$foreground,
    mapsf.background   = theme$background,
    mapsf.highlight    = theme$highlight,
    mapsf.pal_quali    = theme$pal_quali,
    mapsf.pal_seq      = theme$pal_seq,
    mapsf.legacy       = theme$legacy
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
