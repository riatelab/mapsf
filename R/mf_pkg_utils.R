
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



# #' @importFrom magrittr %>%
# #' @export
# magrittr::`%>%`

#' @importFrom utils globalVariables
.gmapsf <- new.env(parent = emptyenv())
globalVariables(".gmapsf", package = "mapsf", add = FALSE)
.gmapsf$args <- list(
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
)
.gmapsf$inset <- FALSE
