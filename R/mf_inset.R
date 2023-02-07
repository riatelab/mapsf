#' @title Plot an inset
#' @description This function is used to add an inset map to the current map.
#' @param x an sf object, or "worldmap" to use with \link{mf_worldmap}.
#' @param pos position, one of "bottomleft", "left", "topleft", "top", "bottom",
#' "bottomright", "right", "topright"
#' @param cex share of the map width occupied by the inset
#' @param fig coordinates of the inset region (in NDC, see in ?par())
#' @note This function does not work when mfrow is used in par().
#' @export
#' @details
#' If x is used (with pos and cex), the width/height ratio of the inset will
#' match the width/height ratio of x bounding box.\cr
#' If fig is used, coordinates (xmin, xmax, ymin, ymax) are expressed as
#' fractions of the mapping space (i.e. excluding margins).\cr
#' If map layers have to be plotted after the inset (i.e after mf_inset_off()),
#' please use add = TRUE.\cr
#' It is not possible to plot an inset within an inset.\cr
#' It is possible to plot anything (base plots) within the inset, not only map layers.
#' @return No return value, an inset is initiated or closed.
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_inset_on(x = mtq[1, ], cex = .2)
#' mf_map(mtq[1, ])
#' mf_inset_off()
#'
#' mf_map(mtq)
#' mf_inset_on(x = "worldmap", pos = "bottomleft")
#' mf_worldmap(x = mtq)
#' mf_inset_off()
#'
#' mf_map(mtq)
#' mf_inset_on(fig = c(0, 0.25, 0, 0.25))
#' mf_map(x = mtq)
#' mf_inset_off()
mf_inset_on <- function(x, pos = "topright", cex = .2, fig) {
  if (.gmapsf$inset) {
    stop(
      paste0(
        "You have already started an inset. ",
        "Use `mf_inset_off()` to close the current inset."
      ),
      call. = FALSE
    )
  }
  .gmapsf$inset <- TRUE
  pmar <- par("mar")
  pusr <- par("usr")
  .gmapsf$par <- par(no.readonly = TRUE)
  if (!missing(x)) {
    if (is.character(x)) {
      if (x == "worldmap") {
        h <- 1
        w <- 1
      }
    } else {
      bb <- st_bbox(x)
      w <- bb[3] - bb[1]
      h <- bb[4] - bb[2]
    }
    ratio <- h / w
    wdest <- (pusr[2] - pusr[1]) * cex
    hdest <- wdest * ratio
    inset_xy <- posinset(pos = pos, pusr = pusr, wdest = wdest, hdest = hdest)
    tgx <- c(
      grconvertX(inset_xy[1:2], from = "user", to = "ndc"),
      grconvertY(inset_xy[3:4], from = "user", to = "ndc")
    )
  }

  if (!missing(fig)) {
    ffig <- c(
      grconvertX(pusr[1:2], from = "user", to = "ndc"),
      grconvertY(pusr[3:4], from = "user", to = "ndc")
    )
    tgtx <- fig[1:2]
    tgty <- fig[3:4]

    ffig2 <- c(tgtx * (ffig[2] - ffig[1]), tgty * (ffig[4] - ffig[3]))
    tgx <- c(ffig[1] + ffig2[1:2], ffig[3] + ffig2[3:4])
  }

  theme <- mf_theme()
  .gmapsf$oth <- theme


  if (max(tgx) > 1 | min(tgx) < 0) {
    tgx[tgx > 1] <- 1
    tgx[tgx < 0] <- 0
  }
  par(bg = NA)
  par(
    fig = round(tgx, 3),
    mar = c(0, 0, 0, 0),
    new = TRUE
  )
  theme$mar <- c(0, 0, 0, 0)
  theme$bg <- NA
  mf_theme(theme)
}


#' @name mf_inset_on
#' @export
mf_inset_off <- function() {
  .gmapsf$inset <- FALSE
  pusr <- .gmapsf$par$usr
  par(.gmapsf$par)
  par(new = TRUE, mar = .gmapsf$oth$mar)
  plot.new()
  plot.window(
    xlim = pusr[1:2], ylim = pusr[3:4],
    xaxs = "i", yaxs = "i", asp = TRUE
  )
  mf_theme(.gmapsf$oth)
  par(mar = .gmapsf$par$mar)
  points(1, 1, col = NA)
}








#' xy of legend
#'
#' @param pos pos
#' @param pu pu
#' @param wdest dl
#' @param hdest dl
#'
#' @noRd
posinset <- function(pos, pusr, wdest, hdest) {
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
  inset <- strwidth("M", units = "user", cex = 1) / 2
  pusr <- pusr + c(inset, -inset, inset, -inset)

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
  xy
}
