#' @title Get class intervals
#' @name mf_get_breaks
#' @description A function to classify continuous variables.
#' @param x a vector of numeric values. NA and Inf values are not used in the
#' classification.
#' @param nbreaks a number of classes
#' @param breaks a classification method; one of "fixed", "sd", "equal",
#' "pretty", "quantile",
#' "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "q6", "geom",
#'  "arith", "em" or "msd" (see Details).
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
#' Jenks ("jenks" method) and Fisher ("fisher" method) algorithms are
#' based on the same principle and give
#' quite similar results but Fisher is much faster. \cr\cr
#' The "q6" method uses the following \code{\link[stats:quantile]{quantile}}
#' probabilities: 0, 0.05, 0.275, 0.5, 0.725, 0.95, 1.\cr\cr
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
#' @note This function is mainly a wrapper
#' of \code{\link[classInt:classIntervals]{classIntervals}} +
#' "arith", "em", "q6", "geom" and "msd" methods.
#' @examples
#' mtq <- mf_get_mtq()
#' mf_get_breaks(x = mtq$MED, nbreaks = 6, breaks = "quantile")
#' @return A numeric vector of breaks
#' @export
mf_get_breaks <- function(x, nbreaks, breaks, k = 1, central = FALSE, ...) {
  if (is.numeric(breaks)) {
    intervals <- sort(breaks)
    return(intervals)
  }

  x <- as.vector(na.omit(x))
  x <- x[is.finite(x)]
  custom_methods <- c("geom", "arith", "q6", "em", "msd")

  # default number of classes
  if (missing(nbreaks)) {
    nbreaks <- round(1 + 3.3 * log10(length(x)), 0)
  }

  if (!breaks %in% custom_methods) {
    intervals <- classInt::classIntervals(
      var = x,
      n = nbreaks,
      style = breaks, ...
    )$brks
  } else {
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
      intervals <- as.vector(quantile(x,
        probs =
          c(0, 5, 27.5, 50, 72.5, 95, 100) / 100
      ))
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
  }
  return(unique(intervals))
}
