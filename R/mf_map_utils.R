#' Title
#'
#' @param pal pal
#' @param nbreaks nbreaks
#' @param alpha alpha
#' @noRd
#' @importFrom grDevices hcl.pals hcl.colors
get_the_pal <- function(pal, nbreaks, alpha = 1, rev = TRUE) {
  if (length(pal) == 1) {
    if (pal %in% hcl.pals()) {
      cols <- hcl.colors(n = nbreaks, palette = pal, alpha = alpha, rev = rev)
    } else {
      cols <- rep(pal, nbreaks)
    }
  } else {
    cols <- pal[1:nbreaks]
  }
  return(cols)
}

get_col_vec <- function(x, breaks, pal, jen = FALSE) {
  if (jen) {
    itv <- apply(array(apply(outer(x, breaks, ">"), 1, sum)), 1, max, 1)
  } else {
    itv <- findInterval(x, breaks, all.inside = FALSE, rightmost.closed = TRUE)
  }

  lb <- length(breaks)
  nout <- sum(itv %in% c(0, lb))
  if (nout > 0) {
    if (nout == 1) {
      message(paste0(
        "1 value is outside the class limits and",
        " will be classified as 'NA'."
      ))
    } else {
      message(paste0(
        nout, " values are outside the class limits and will",
        " be classified as 'NA'."
      ))
    }
  }

  itv[itv == 0] <- length(breaks)
  colvec <- pal[itv]
  return(colvec)
}



#' @name create_dots
#' @title create_dots
#' @description Create clean, sorted sf object with centroid coordinates from
#' an sf object
#' @param x x
#' @param var var
#' @return A sorted and cleaned sf object with centroid coordinates.
#' @noRd
create_dots <- function(x = x, var = var) {
  # get centroid coords
  x <- cbind(
    st_coordinates(st_centroid(
      x = st_geometry(x),
      of_largest_polygon = TRUE
    )),
    x
  )
  lx <- nrow(x)
  # remove NAs and 0 values
  x <- x[!is.na(x = x[[var]]), ]
  lxna <- nrow(x)
  x <- x[x[[var]] != 0, ]
  lx0 <- nrow(x)

  x <- x[is.finite(x[[var]]), ]
  lxinf <- nrow(x)


  nna <- lx - lxna
  nn0 <- lx - nna - lx0
  nni <- lx - nna - nn0 - lxinf

  if (nna > 0) {
    if (nna == 1) {
      message("1 'NA' value is not plotted on the map.")
    } else {
      message(paste0(nna, " 'NA' values are not plotted on the map."))
    }
  }
  if (nn0 > 0) {
    if (nn0 == 1) {
      message("1 '0' value is not plotted on the map.")
    } else {
      message(paste0(nn0, " '0' values are not plotted on the map."))
    }
  }

  if (nni > 0) {
    if (nni == 1) {
      message("1 'Infinite' value is not plotted on the map.")
    } else {
      message(paste0(nni, " 'Infinite' values are not plotted on the map."))
    }
  }

  # check if there is something to plot
  if (nrow(x) == 0) {
    stop("Nothing is plotted on the map.", call. = FALSE)
  }

  # turn to positive values
  if (min(x[[var]]) < 0) {
    message("Negative values have been transformed into positive values.")
    x[[var]] <- abs(x[[var]])
  }

  # Order the dots
  x <- x[order(x[[var]], decreasing = TRUE), ]
  return(x)
}





#' @name get_size
#' @title get_size
#' @description get a vector of radii
#' @param inches inches
#' @param var var
#' @param fixmax fixmax
#' @param symbol symbols
#' @return a vector of radii
#' @noRd
get_size <- function(var, inches, val_max, symbol) {
  switch(symbol,
    circle = {
      smax <- inches * inches * pi
      size <- sqrt((var * smax / val_max) / pi)
    },
    square = {
      smax <- inches * inches
      size <- sqrt(var * smax / val_max)
    }
  )
  return(size)
}





# Plot symbols
plot_symbols <- function(symbol, dots, sizes, mycols, border, lwd, inches) {
  if (inherits(dots, c("sf", "sfc"))) {
    xy <- sf::st_set_geometry(dots[, 1:2], NULL)
  } else {
    xy <- dots
  }
  switch(symbol,
    circle = {
      symbols(
        x = xy[, 1],
        y = xy[, 2],
        circles = sizes,
        bg = mycols,
        fg = border,
        lwd = lwd,
        add = TRUE,
        inches = inches,
        asp = 1
      )
    },
    square = {
      symbols(
        x = xy[, 1],
        y = xy[, 2],
        squares = sizes,
        bg = mycols,
        fg = border,
        lwd = lwd,
        add = TRUE,
        inches = inches * 2,
        asp = 1
      )
    }
  )
}



check_order <- function(val_order, mod) {
  if (missing(val_order)) {
    val_order <- sort(mod)
  }
  return(val_order)
}


get_modalities <- function(x, val_order) {
  mod <- unique(x)
  mod <- mod[!is.na(mod)]
  # check val_order vs mod values
  val_order <- check_order(val_order, mod)
}

get_col_typo <- function(x, pal, val_order) {
  # get the colors
  refcol <- data.frame(
    mod = val_order,
    col = pal,
    stringsAsFactors = FALSE
  )
  mycols <- refcol[match(x, refcol[, 1]), 2]
  mycols
}

get_sym_typo <- function(x, pch, val_order) {
  # get the colors
  refsym <- data.frame(
    mod = val_order,
    pch = pch,
    stringsAsFactors = FALSE
  )
  mysym <- refsym[match(x, refsym[, 1]), 2]
  mysym
}


# split multiple legend position
split_leg <- function(x) {
  llp <- length(x)
  if (llp == 1) {
    return(list(l1 = x))
  }
  if (llp == 2) {
    lp1 <- x[1]
    lp2 <- x[2]
    if (is.numeric(c(lp1, lp2))) {
      return(list(l1 = c(lp1, lp2)))
    }
  }
  if (llp == 3) {
    tt <- tryCatch(as.numeric(x[1]), warning = function(w) w)
    if (inherits(tt, "warning")) {
      lp1 <- x[1]
      lp2 <- as.numeric(x[2:3])
    } else {
      lp1 <- as.numeric(x[1:2])
      lp2 <- x[3]
    }
  }
  if (llp == 4) {
    lp1 <- x[1:2]
    lp2 <- x[3:4]
  }
  return(list(l1 = lp1, l2 = lp2))
}



get_geom_type <- function(x) {
  a <- list(
    POINT = "POINT",
    POINT = "MULTIPOINT",
    LINE = "LINESTRING",
    LINE = "MULTILINESTRING",
    POLYGON = "POLYGON",
    POLYGON = "MULTIPOLYGON",
    other = "GEOMETRY",
    other = "GEOMETRYCOLLECTION",
    other = "CIRCULARSTRING",
    other = "COMPOUNDCURVE",
    other = "CURVEPOLYGON",
    other = "MULTICURVE",
    other = "MULTISURFACE",
    other = "CURVE",
    other = "SURFACE",
    other = "POLYHEDRALSURFACE",
    other = "TIN",
    other = "TRIANGLE"
  )
  type <- st_geometry_type(x)
  levels(type) <- a
  type <- as.character(unique(type))
  if (length(type) > 1) {
    stop("GEOMETRYCOLLECTION objects should have consistent type",
      call. = FALSE
    )
  }
  return(type)
}


# arg checking depending on type
check_args <- function(argx, type) {
  n_rel <- !names(argx) %in% names(formals(get(paste0("mf_", type))))
  s_n_rel <- sum(n_rel)
  if (s_n_rel >= 1) {
    mes <- "The following arguments are not relevant when using type = '"
    if (s_n_rel == 1) {
      mes <- "The following argument is not relevant when using type = '"
    }
    message(paste0(
      mes, type, "': ",
      paste0(names(argx[n_rel]), collapse = ", "),
      "."
    ))
    argx <- argx[!n_rel]
  }
  argx
}
