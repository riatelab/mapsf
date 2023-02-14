#' @title Plot graduated symbols
#' @description Plot graduated symbols based on quantitative data.
#' @eval my_params(c(
#' 'x',
#' 'var',
#' 'border',
#' 'lwd',
#' 'cexs',
#' 'pch',
#' 'add' ,
#' 'col',
#' 'leg_pos',
#' 'leg_title',
#' 'leg_title_cex',
#' 'leg_val_cex',
#' 'leg_val_rnd',
#' 'leg_frame',
#' 'breaks',
#' 'nbreaks'))
#' @details
#' Breaks defined by a numeric vector or a classification method are
#' left-closed: breaks defined by \code{c(2, 5, 10, 15, 20)}
#' will be mapped as [2 - 5[, [5 - 10[, [10 - 15[, [15 - 20].
#' The "jenks" method is an exception and has to be right-closed.
#' Jenks breaks computed as \code{c(2, 5, 10, 15, 20)}
#' will be mapped as [2 - 5], ]5 - 10], ]10 - 15], ]15 - 20].
#' @importFrom methods is
#' @importFrom graphics box
#' @keywords internal
#' @export
#' @return x is (invisibly) returned.
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_grad(mtq, "POP", pch = 22)
mf_grad <- function(x,
                    var,
                    breaks = "quantile",
                    nbreaks = 3,
                    col = "tomato4",
                    border,
                    pch = 21,
                    cex,
                    lwd,
                    leg_pos = mf_get_leg_pos(x),
                    leg_title = var,
                    leg_title_cex = .8,
                    leg_val_cex = .6,
                    leg_val_rnd = 2,
                    leg_frame = FALSE,
                    add = TRUE) {
  # default
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))
  bg <- getOption("mapsf.bg")
  fg <- getOption("mapsf.fg")
  if (missing(border)) border <- fg
  xout <- x

  # data prep
  x <- x[!is.na(x = x[[var]]), ]
  x <- x[order(x[[var]], decreasing = TRUE), ]
  # jenks
  jen <- FALSE
  if (any(breaks %in% "jenks")) {
    jen <- TRUE
  }
  breaks <- mf_get_breaks(x = x[[var]], nbreaks = nbreaks, breaks = breaks)
  nbreaks <- length(breaks) - 1

  xtype <- get_geom_type(x)
  if (xtype == "LINE") {
    # lwd mgmt
    if (missing(lwd)) {
      lwd <- seq(1, 4, length.out = nbreaks)
    }
    if (length(lwd) != nbreaks) {
      stop(paste0(
        "the length of lwd does not match the number of ",
        "breaks."
      ), call. = FALSE)
    }
    mylwd <- get_col_vec(
      x = x[[var]], breaks = breaks, pal = lwd, jen = jen
    )

    if (add == FALSE) {
      mf_init(x)
      add <- TRUE
    }
    # map
    plot(sf::st_geometry(x), col = col, lwd = mylwd, add = add)
    # legend
    mf_legend_gl(
      pos = leg_pos, val = breaks, title = leg_title,
      title_cex = leg_title_cex, val_cex = leg_val_cex,
      val_rnd = leg_val_rnd, lwd = lwd, col = col,
      bg = bg, fg = fg, frame = leg_frame
    )
    return(invisible(xout))
  }

  if (missing(lwd)) lwd <- .7

  # Transform to point
  st_geometry(x) <- st_centroid(st_geometry(x), of_largest_polygon = TRUE)
  # cex mgmt
  if (missing(cex)) {
    cex <- seq(1, 4, length.out = nbreaks)
  }
  if (length(cex) != nbreaks) {
    stop(paste0(
      "the length of cex does not match the number of ",
      "breaks."
    ), call. = FALSE)
  }
  mycex <- get_col_vec(
    x = x[[var]], breaks = breaks, pal = cex, jen = jen
  )
  # color mgmt
  pch <- pch[1]
  mycolspt <- col[1]
  if (pch %in% 21:25) mycolspt <- border
  mycolsptbg <- col[1]


  if (add == FALSE) {
    mf_init(x)
    add <- TRUE
  }
  # display
  plot(st_geometry(x),
    col = mycolspt, bg = mycolsptbg, cex = mycex, pch = pch,
    lwd = lwd, add = add
  )
  # legend
  lb <- length(breaks)
  lab <- paste0(breaks[1:(lb - 1)], rep(" - ", lb - 1), breaks[2:lb])
  mf_legend_s(
    pos = leg_pos, val = rev(lab), title = leg_title, title_cex = leg_title_cex,
    val_cex = leg_val_cex,
    frame = leg_frame, border = border, pal = col, lwd = lwd,
    pt_cex = rev(cex), pt_pch = pch, bg = bg, fg = fg
  )
  return(invisible(xout))
}
