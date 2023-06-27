#
# import stuffs
# @import graphics
# @import stats
#' @import sf
#' @importFrom grDevices colorRampPalette
#' @importFrom stats aggregate median na.omit quantile runif sd


# Rcpp stuff
#' @useDynLib mapsf,.registration = TRUE
#' @importFrom Rcpp evalCpp
NULL

#' @importFrom utils globalVariables
.gmapsf <- new.env(parent = emptyenv())
globalVariables(".gmapsf", package = "mapsf", add = FALSE)
.gmapsf$inset <- FALSE
.gmapsf$positions <- c(
  "bottomleft", "left", "topleft", "top", "bottom",
  "bottomright", "right", "topright",
  "bottomleft1", "bottomright1", "bottom1",
  "bottomleft2", "bottomright2", "bottom2",
  "topright1", "topleft1", "top1",
  "topright2", "topleft2", "top2",
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


plot_is_lonlat <- function(type) {
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  opt <- options(error = NULL)
  on.exit(options(opt), add = TRUE)
  on.exit(par(op), add = TRUE)
  usr <- par("usr")
  pin <- par("pin")
  if (diff(usr[1:2] / pin[1]) - diff(usr[3:4] / pin[2]) >= 10e-4) {
    if (type == "error") {
      stop(paste0(
        "This feature only works with projected layers.\n",
        "It seems that you are using an unprojected geographic layer\n",
        "(using longitude and latitude).\n",
        "You can use crssuggest::suggest_crs(x) to find a candidate CRS,\n",
        "then sf::st_tranform(x, 'crs_code') to transform the layer."
      ), call. = FALSE)
    }
    if (type == "message") {
      message(paste0(
        "Most cartographic features work better with projected layers.\n",
        "It seems that you are using an unprojected geographic layer\n",
        "(using longitude and latitude).\n",
        "You can use crssuggest::suggest_crs(x) to find a candidate CRS,\n",
        "then sf::st_tranform(x, 'crs_code') to transform the layer."
      ))
    }
  }
}
