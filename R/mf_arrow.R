#' @title Plot a north arrow
#' @description Plot a north arrow.
#' @name mf_arrow
#' @eval my_params(c('pos'))
#' @param col arrow color
#' @param cex arrow size
#' @param adj adjust the postion of the north arrow in x and y directions
#' @param align object of class \code{sf} or \code{sfc} used to adjust the
#' arrow to the real north
#' @param adjust deprecated, see align
#' @importFrom sf st_crs st_as_sf st_coordinates st_transform
#' @return No return value, a north arrow is displayed.
#' @export
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_arrow(pos = "topright")
mf_arrow <- function(pos = "topleft", col = getOption("mapsf.fg"),
                     cex = 1,
                     adj = c(0, 0),
                     align, adjust) {
  test_cur_plot()
  if (!missing(adjust)) {
    warning(paste0("'adjust' is deprecated.\nUse 'align' instead."),
      call. = FALSE
    )
    align <- adjust
  }

  if (missing(col)) {
    col <- getOption("mapsf.fg")
  }

  map_extent <- par("usr")
  xe <- map_extent[1:2]
  ye <- map_extent[3:4]
  inset <- xinch(par("csi")) / 2
  n_arrow <- build_arrow(mean(xe), mean(ye), inset * cex)
  bb_n_arrow <- st_bbox(n_arrow)
  h <- bb_n_arrow[4] - bb_n_arrow[2]
  w <- bb_n_arrow[3] - bb_n_arrow[1]
  xe <- xe + c(inset, -inset) / 2
  ye <- ye + c(inset, -inset) / 2
  pos_a <- get_arrow_pos(pos, xe, ye, w, h) + adj * inset / 2
  north_arrow <- n_arrow + c(pos_a[1] - bb_n_arrow[1], pos_a[2] - bb_n_arrow[4])

  if (!missing(align)) {
    xcrs <- st_crs(align)
    a <- st_as_sf(
      x = data.frame(X = pos_a[1], Y = pos_a[2]), coords = c("X", "Y"),
      crs = xcrs, remove = FALSE
    )
    b <- st_as_sf(
      x = data.frame(st_coordinates(a) + c(0, -100000)),
      coords = c("X", "Y"), crs = xcrs, remove = FALSE
    )
    ap <- st_transform(a, 4326)
    cp <- st_as_sf(
      x = data.frame(st_coordinates(ap) + c(0, -2)),
      coords = c("X", "Y"), crs = 4326, remove = FALSE
    )
    cx <- st_transform(cp, xcrs)
    cx[, c("X", "Y")] <- st_coordinates(cx)
    delta_xb <- b$X - a$X
    delta_yb <- b$Y - a$Y
    delta_xc <- cx$X - a$X
    delta_yc <- cx$Y - a$Y
    v_x <- c(delta_xb, delta_yb)
    v_y <- c(delta_xc, delta_yc)
    theta <- acos(sum(v_x * v_y) / (sqrt(sum(v_x^2)) * sqrt(sum(v_y^2))))
    theta <- sign(cx$X - b$X) * theta
    rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
    n_arrow <- north_arrow * rot(-theta)
    bb_n_arrow <- st_bbox(n_arrow)
    h <- bb_n_arrow[4] - bb_n_arrow[2]
    w <- bb_n_arrow[3] - bb_n_arrow[1]
    pos_a <- get_arrow_pos(pos, xe, ye, w, h)
    north_arrow <- n_arrow + c(pos_a[1] - bb_n_arrow[1],
                               pos_a[2] - bb_n_arrow[4])
  }

  mf_map(north_arrow, col = col, border = col, add = TRUE)
}

get_arrow_pos <- function(pos, xe, ye, w, h) {
  if (is.numeric(pos) && length(pos) == 2) {
    xarrow <- pos[1]
    yarrow <- pos[2]
  } else {
    switch(pos,
      topleft = {
        xarrow <- xe[1]
        yarrow <- ye[2]
      },
      bottomright = {
        xarrow <- xe[2] - w
        yarrow <- ye[1] + h
      },
      topright = {
        xarrow <- xe[2] - w
        yarrow <- ye[2]
      },
      bottomleft = {
        xarrow <- xe[1]
        yarrow <- ye[1] + h
      },
      top = {
        xarrow <- xe[1] + diff(xe) / 2 - w * .5
        yarrow <- ye[2]
      },
      bottom = {
        xarrow <- xe[1] + diff(xe) / 2 - w
        yarrow <- ye[1] + h
      },
      left = {
        xarrow <- xe[1]
        yarrow <- ye[1] + diff(ye) / 2 + h * 0.5
      },
      right = {
        xarrow <- xe[2] - w
        yarrow <- ye[1] + diff(ye) / 2 + h * 0.5
      },
      interactive = {
        iar <- interleg(txt = c("arrow", "Arrow"))
        xarrow <- iar[1]
        yarrow <- iar[2]
      }
    )
  }
  return(c(xarrow, yarrow))
}

build_arrow <- function(x, y, inset) {
  x_triangle <- c(
    x,
    x + inset / 2,
    x + inset,
    x + inset / 2,
    x
  )
  y_triangle <- c(
    y - inset,
    y,
    y - inset,
    y - inset * .9,
    y - inset
  )
  triangle <- st_polygon(list(matrix(
    data = c(x_triangle, y_triangle),
    nrow = 5, ncol = 2, byrow = FALSE
  )))
  x_n <- c(
    x + inset / 4,
    x + inset / 4,
    x + inset / 4 + inset / 2,
    x + inset / 4 + inset / 2
  )
  y_n <- c(
    y - inset - inset * .75 - inset / 3,
    y - inset - inset / 3,
    y - inset - inset * .75 - inset / 3,
    y - inset - inset / 3
  )
  n <- st_multilinestring(list(matrix(
    data = c(x_n, y_n),
    nrow = 4, ncol = 2,
    byrow = FALSE
  )))
  n <- st_buffer(n, inset * 0.05,
    endCapStyle = "SQUARE",
    joinStyle = "MITRE", mitreLimit = 1
  )

  n_arrow <- st_multipolygon(list(triangle, n))

  return(n_arrow)
}
