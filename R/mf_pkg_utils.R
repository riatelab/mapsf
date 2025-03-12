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
