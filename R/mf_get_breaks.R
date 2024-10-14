#' @title Get class intervals
#' @name mf_get_breaks
#' @description A function to classify continuous variables.
#'
#' This function is a wrapper for
#' \code{\link[classInt:classIntervals]{classIntervals}}
#' with some additional methods.
#'
#' @param x a vector of numeric values. NA and Inf values are not used in the
#' classification.
#' @param nbreaks a number of classes
#' @param breaks a classification method; one of "fixed", "sd", "equal",
#' "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks",
#' "dpih", "q6", "Q6", geom", "arith", "em", "msd" or "ckmeans" (see Details)
#' @param k number of standard deviation for "msd" method (see Details)
#' @param central creation of a central class for "msd" method (see Details)
#' @param ... further arguments
#' of \code{\link[classInt:classIntervals]{classIntervals}}
#' @seealso \link[classInt:classIntervals]{classIntervals}
#' @details
#' "fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust",
#' "bclust", "fisher", "jenks" and "dpih"
#' are \code{\link[classInt:classIntervals]{classIntervals}}
#' methods. You may need to pass additional arguments for some of them.\cr\cr
#'
#' The "jenks", "fisher" and "ckmeans" methods are based on the same concept of
#' **natural breaks** and and produce similar groupings.
#'
#' * The "jenks" method produces class boundaries falling on data points and is
#' slow.
#' * The "fisher" method produces class boundaries located more conveniently
#' between data points, and is faster than the "jenks" method.
#' * The "ckmeans" method produces exactly the same class boundaries as the
#' "fisher" method, but is much faster. It uses the optimal univariate
#' k-means method from the `Ckmeans.1d.dp` package.
#' If the "ckmeans" method is selected but the `Ckmeans.1d.dp` package is not
#' installed then the "fisher" method is used.
#'
#' The relative speeds of these three methods may vary depending on the number
#' of data points and the number of classes.\cr\cr
#'
#' The "q6" method uses the following \code{\link[stats:quantile]{quantile}}
#' probabilities: 0, 0.05, 0.275, 0.5, 0.725, 0.95, 1.\cr\cr
#' The "Q6" method uses the following \code{\link[stats:quantile]{quantile}}
#' probabilities: 0, 0.05, 0.25, 0.5, 0.75, 0.95, 1.\cr\cr
#' The "geom" method is based on a geometric progression along
#' the variable values, all values must be strictly greater than zero.\cr\cr
#' The "arith" method is based on an arithmetic progression along
#' the variable values.\cr\cr
#' The "em" method is based on nested averages computation.\cr\cr
#' The "msd" method is based on the mean and the standard deviation
#' of a numeric vector.
#' The \code{nbreaks} parameter is not relevant, use \code{k} and
#' \code{central} instead. \code{k} indicates
#' the extent of each class in share of standard deviation.
#' If \code{central=TRUE} then
#' the mean value is the center of a class else the mean is a break value.
#' @examples
#' mtq <- mf_get_mtq()
#' mf_get_breaks(x = mtq$MED, nbreaks = 6, breaks = "quantile")
#' @return A numeric vector of breaks
#' @md
#' @export
mf_get_breaks <- function(x, nbreaks, breaks, k = 1, central = FALSE, ...) {
  if (is.numeric(breaks)) {
    intervals <- sort(breaks)
    return(intervals)
  }

  x <- as.vector(na.omit(x))
  x <- x[is.finite(x)]
  custom_methods <- c("geom", "arith", "q6", "Q6", "em", "msd", "ckmeans")

  # default number of classes
  if (missing(nbreaks)) {
    nbreaks <- round(1 + 3.3 * log10(length(x)), 0)
  }

  if (breaks == "ckmeans") {
    if (!requireNamespace("Ckmeans.1d.dp", quietly = TRUE)) {
      warning(
        paste0(
          "The 'Ckmeans.1d.dp' package is needed for ",
          "this classification method. Please install it.\n",
          "The 'fisher' method will be used instead."
        ),
        call. = FALSE
      )
      breaks <- "fisher"
    } else {
      bks <- Ckmeans.1d.dp::Ckmeans.1d.dp(x = x, k = nbreaks)
      intervals <- Ckmeans.1d.dp::ahist(
        x = bks, style = "midpoints",
        data = x, plot = FALSE
      )$breaks
    }
  }

  if (!breaks %in% custom_methods) {
    intervals <- classInt::classIntervals(
      var = x,
      n = nbreaks,
      style = breaks, ...
    )$brks
  }

  if (breaks == "geom") {
    intervals <- min(x)
    if (intervals <= 0) {
      stop(
        paste0(
          "All values must be strictly greater ",
          "than 0 when using the 'geom' method."
        ),
        call. = FALSE
      )
    }
    intervals <- c(intervals, max(x))
    r <- exp((log(max(x)) - log(min(x))) / nbreaks) # raison
    tmp <- min(x)
    for (i in 1:(nbreaks - 1)) {
      intervals <- c(intervals, tmp * r)
      tmp <- tmp * r
      intervals <- sort(intervals)
    }
  }

  if (breaks == "arith") {
    intervals <- min(x)
    intervals <- c(intervals, max(x))
    r <- (max(x) - min(x)) / sum(1:nbreaks) # raison
    tmp <- min(x)
    for (i in 1:(nbreaks - 1)) {
      intervals <- c(intervals, tmp + r)
      tmp <- tmp + r
      intervals <- sort(intervals)
    }
  }

  if (breaks == "q6") {
    intervals <- as.vector(
      quantile(x = x, probs = c(0, .05, .275, .5, .725, .95, 1))
    )
  }

  if (breaks == "Q6") {
    intervals <- as.vector(
      quantile(x = x, probs = c(0, .05, .25, .5, .75, .95, 1))
    )
  }

  if (breaks == "em") {
    t <- bitwAnd(nbreaks, (nbreaks - 1))
    if (t != 0) {
      stop("The number of classes must be a power of 2")
    } else {
      min_vec <- min(x)
      max_vec <- max(x)
      it <- log2(nbreaks)

      int <- c(min_vec, max_vec)

      for (a in 1:it) {
        valprv <- c()
        for (i in 1:(length(int) - 1)) {
          if (i == 1) {
            sub_vec <- x[x >= int[i] & x <= int[i + 1]]
          } else {
            sub_vec <- x[x > int[i] & x <= int[i + 1]]
          }
          valprv <- c(valprv, mean(sub_vec))
        }
        int <- c(int, valprv)
        int <- int[order(int)]
      }
      intervals <- int
    }
  }

  if (breaks == "msd") {
    min_vec <- min(x)
    max_vec <- max(x)
    avg_vec <- mean(x)
    sd_vec <- sqrt(sum((x - avg_vec)^2) / length(x))
    if (central == FALSE) {
      pose <- ceiling((max_vec - avg_vec) / (sd_vec * k))
      nege <- ceiling((avg_vec - min_vec) / (sd_vec * k))
      avg_vec + (1:pose) * (sd_vec * k)
      bks <- c(
        avg_vec - (1:nege) * (sd_vec * k),
        avg_vec,
        avg_vec + (1:pose) * (sd_vec * k)
      )
      intervals <- c(min_vec, bks[bks > min_vec & bks < max_vec], max_vec)
    } else {
      pose <- ceiling((max_vec - (avg_vec + 0.5 * sd_vec * k)) / (sd_vec * k))
      nege <- ceiling(((avg_vec - 0.5 * sd_vec * k) - min_vec) / (sd_vec * k))
      bks <- c(
        (avg_vec - 0.5 * sd_vec * k) - (1:nege) * (sd_vec * k),
        (avg_vec - 0.5 * sd_vec * k),
        (avg_vec + 0.5 * sd_vec * k),
        (avg_vec + 0.5 * sd_vec * k) + (1:pose) * (sd_vec * k)
      )
      intervals <- c(min_vec, bks[bks > min_vec & bks < max_vec], max_vec)
    }
    intervals <- intervals[order(intervals)]
  }

  return(unique(intervals))
}
