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


get_breaks_methods_names <- function() {
  return(
    c(
      "quantile", "equal", "msd", "ckmeans", "Q6", "geom", "fixed", "sd",
      "pretty", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih",
      "headtails", "maximum", "box", "q6", "arith", "em"
    )
  )
}


get_themes_names <- function() {
  n <- names(.gmapsf$themes)
  return(n[!n %in% c(
    "default", "brutal", "ink", "dark", "agolalight", "candy",
    "darkula", "iceberg", "green", "nevermind", "jsk", "barcelona"
  )])
}


readimage <- function(filename){
  ex <- strsplit(basename(filename), split = "\\.")[[1]]
  ex <- tolower(ex[length(ex)])
  if (ex == "png") {
    if (!requireNamespace("png", quietly = TRUE)) {
      stop(
        "'png' is package needed for this function to work. Please install it.",
        call. = FALSE
      )
    }
    img <- png::readPNG(filename)
  }
  if (ex %in% c("jpg", "jpeg")) {
    if (!requireNamespace("jpeg", quietly = TRUE)) {
      stop(
        paste0(
          "'jpeg' is package needed for this function to work. ",
          "Please install it."
        ),
        call. = FALSE
      )
    }
    img <- jpeg::readJPEG(filename)
  }
  return(img)
}


#' xy of legend
#'
#' @param pos pos
#' @param pu pu
#' @param wdest dl
#' @param hdest dl
#'
#' @noRd
posinset <- function(pos, pusr, wdest, hdest, adj = c(0,0)) {
  if (is.numeric(pos) && length(pos) == 2) {
    xy <- c(pos[1],
            pos[1] + wdest,
            pos[2] - hdest,
            pos[2])
    return(xy)
  }

  posposs <- c(
    "bottomleft", "left", "topleft", "top", "bottom",
    "bottomright", "right", "topright"
  )
  if (!pos %in% posposs) {
    stop(paste0(
      "pos should be one of ", paste0(posposs, collapse = ", "),
      "."
    ), call. = FALSE)
  }

  x_spacing <- xinch(par("csi")) / 4
  y_spacing <- yinch(par("csi")) / 4
  pusr <- pusr + c(x_spacing, -x_spacing, y_spacing, -y_spacing)

  xy <- switch(pos,
               bottomleft = c(
                 pusr[1],
                 pusr[1] + wdest,
                 pusr[3],
                 pusr[3] + hdest
               ),
               topleft = c(
                 pusr[1],
                 pusr[1] + wdest,
                 pusr[4] - hdest,
                 pusr[4]
               ),
               left = c(
                 pusr[1],
                 pusr[1] + wdest,
                 pusr[3] + (pusr[4] - pusr[3]) / 2 - (hdest) / 2,
                 pusr[3] + (pusr[4] - pusr[3]) / 2 + (hdest) / 2
               ),
               top = c(
                 pusr[1] + (pusr[2] - pusr[1]) / 2 - (wdest) / 2,
                 pusr[1] + (pusr[2] - pusr[1]) / 2 + (wdest) / 2,
                 pusr[4] - hdest,
                 pusr[4]
               ),
               bottom = c(
                 pusr[1] + (pusr[2] - pusr[1]) / 2 - (wdest) / 2,
                 pusr[1] + (pusr[2] - pusr[1]) / 2 + (wdest) / 2,
                 pusr[3],
                 pusr[3] + hdest
               ),
               bottomright = c(
                 pusr[2] - wdest,
                 pusr[2],
                 pusr[3],
                 pusr[3] + hdest
               ),
               right = c(
                 pusr[2] - wdest,
                 pusr[2],
                 pusr[3] + (pusr[4] - pusr[3]) / 2 - (hdest) / 2,
                 pusr[3] + (pusr[4] - pusr[3]) / 2 + (hdest) / 2
               ),
               topright = c(
                 pusr[2] - wdest,
                 pusr[2],
                 pusr[4] - hdest,
                 pusr[4]
               )
  )
  xy <- xy + c(adj[1], adj[1], adj[2], adj[2]) *
    c(x_spacing, x_spacing, y_spacing, y_spacing)
  return(xy)
}
