test2args <- function(x) {
  if (length(x) != 2) {
    stop(
      paste0(
        "Argument '", deparse(substitute(x)),
        "' needs 2 values."
      ),
      call. = FALSE
    )
  }
  return(x)
}

get_the_pal <- function(pal, nbreaks, alpha, rev = TRUE) {
  if (length(pal) == 1) {
    if (is.function(pal)) {
      cols <- pal(nbreaks)
    } else if (pal %in% hcl.pals()) {
      cols <- hcl.colors(n = nbreaks, palette = pal, rev = rev)
    } else {
      cols <- rep(pal, nbreaks)
    }
  } else {
    cols <- pal[1:nbreaks]
  }
  cols <- get_hex_pal(cols, alpha)
  return(cols)
}

get_hex_pal <- function(pal, alpha) {
  if (!is.null(alpha)) {
    pal <- col2rgb(pal, alpha = FALSE)
    alpha <- sprintf("%02X", as.integer(255.999 * max(min(alpha, 1), 0)))
    ffun <- function(x) {
      rgb(pal[1, x], pal[2, x], pal[3, x], maxColorValue = 255)
    }
    return(paste0(sapply(seq_len(ncol(pal)), ffun), alpha))
  }
  return(pal)
}


get_col_vec <- function(x, breaks, pal) {
  itv <- findInterval(x, breaks, all.inside = FALSE, rightmost.closed = TRUE)
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

create_dots <- function(x = x, var = var) {
  # get centroid coords
  x <- cbind(
    st_coordinates(x),
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


get_point_cols <- function(pch, col, border){
  if (pch %in% 21:25) {
    pt_col <- border
  } else {
    pt_col <- col
  }
  pt_bg <- col
  return(list(col = pt_col, bg = pt_bg))
}


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
    xy <- st_set_geometry(dots[, 1:2], NULL)
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
defunct_leg <- function(x) {
  llp <- length(x)
  if (llp == 1) {
    return(x)
  }
  if (llp == 2) {
    if (is.numeric(x)) {
      return(x)
    }
  }

  stop(paste0(
    "The use of separated legends for this map type is defunct.\n",
    "Please, use only one value for leg_pos",
    " or use mf_legend() to display two legends."
  ), call. = FALSE)
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
  if (length(type) > 1 || type %in% "other") {
    stop(paste0("mapsf support the following geometry types:\n",
                "(MULTI)POINT, (MULTI)LINESTRING and (MULTI)POLYGON.\n",
                "sf::st_cast() and sf::st_collection_extract() can help ",
                "building sf objects with a consistent geometry type."),
      call. = FALSE
    )
  }
  return(type)
}


# arg checking depending on type
check_args <- function(argx, type) {
  n_rel <- !names(argx) %in% names(formals(get(type)))
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


get_val_rnd <- function(val, val_rnd, val_dec = getOption("OutDec"),
                        val_big = "") {
  if (is.numeric(val)) {
    val <- round(val, val_rnd)
    if (val_rnd <= 0) {
      val_rnd <- 0
    }
    val <- format(
      x = val, scientific = FALSE, nsmall = val_rnd,
      decimal.mark = val_dec, big.mark = val_big,
      trim = TRUE
    )
  }
  return(val)
}



get_leg_pos <- function(x, leg_pos) {
  if (length(leg_pos) == 1 && !is.na(leg_pos) && leg_pos == "auto") {
    if (is.null(dev.list())) {
      bb <- st_bbox(x)
    } else {
      p <- par("usr")
      bb <- st_bbox(
        c(
          xmin = p[1], ymin = p[3],
          xmax = p[2], ymax = p[4]
        ),
        crs = st_crs(x)
      )
    }
    g <- st_make_grid(x = st_as_sfc(bb), n = c(3, 3), crs = st_crs(x))
    y <- st_union(st_convex_hull(st_geometry(x)))
    z <- st_intersects(g, y)
    ind <- which(unlist(lapply(z, length)) == 0)
    if (length(ind) > 0) {
      pos <- max(ind)
    } else {
      pos <- which.min(st_area(st_intersection(g, y)))
    }
    tpos <- c(
      "bottomleft", "bottom", "bottomright",
      "left", "center", "right",
      "topleft", "top", "topright"
    )
    return(tpos[pos])
  } else {
    return(leg_pos)
  }
}


get_leg_title <- function(leg_title, var){
  if (length(leg_title) %in% 1:2 && "auto" %in% leg_title){
    return(var)
  }
  return(leg_title)
}

input_validation <- function(x, var, type){

  # Valid x
  cl <- inherits(x = x, what = c("sf", "sfc", "sfg"), which = TRUE) != 0
  if (!any(cl)) {
    stop(paste0("'x' should be an sf, sfc or sfg object."), call. = FALSE)
  }
  if (cl[1] == FALSE && type != "base") {
    stop(paste0("'x' should be an sf object."), call. = FALSE)
  }

  # Valid var
  if (!missing(var)) {
    if (type == "base") {
      message("Please use the 'type' argument to map variables.")
    } else {
      lv <- length(var)
      lin <- var %in% names(x)
      if (lv != length(lin[lin == TRUE])) {
        stop(
          paste0("It is likely that 'var' is not a valid variable name."),
          call. = FALSE
        )
      }
    }
  }

  # Valid type
  if (!type %in% c(
    "base", "prop", "choro", "typo", "symb", "grad",
    "prop_choro", "prop_typo", "symb_choro"
  )) {
    stop(
      paste0(
        '\'type\' should be one of "base", "prop", "choro", "typo", ',
        '"symb", "grad", "prop_choro", "prop_typo" or "symb_choro".'
      ),
      call. = FALSE
    )
  }
  return(NULL)
}

default_add <- function(add, type){
  # add mgmgt, set default add, do not add if no device is launch
  if (missing(add)) {
    add <- switch(type,
                  base = FALSE,
                  choro = FALSE,
                  typo = FALSE,
                  symb = TRUE,
                  prop = TRUE,
                  grad = TRUE,
                  prop_choro = TRUE,
                  prop_typo = TRUE,
                  symb_choro = TRUE
    )

    if (is.null(dev.list())) {
      add <- FALSE
    }
  } else {
    if(is.null(dev.list()) && isTRUE(add)) {
      stop("No geographic layer has been mapped yet. Use add=FALSE.",
           call. = FALSE)
    }
  }

  return(add)
}

geom_preparation <- function(x, type){
  geom_type <- get_geom_type(x)

  if (geom_type %in% c("LINE") && type %in% c("prop_choro", "symb_choro", "symb")){
    stop("This map type is not available for lines.", call. = FALSE)
  }

  if (geom_type %in% c("POLYGON") && type %in% c("prop", "prop_typo", "grad", "prop_choro", "symb_choro", "symb")) {
    st_geometry(x) <- st_centroid(x = st_geometry(x), of_largest_polygon = TRUE)
  }

  return(x)

}





prop_data_cleaning <- function(x = x, var = var) {
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
  if (nrow(x) == 0) {
    stop("Nothing is plotted on the map.", call. = FALSE)
  }
  if (min(x[[var]]) < 0) {
    message("Negative values have been transformed into positive values.")
    x[[var]] <- abs(x[[var]])
  }
  x <- x[order(x[[var]], decreasing = TRUE), ]
  return(x)
}


get_val_max <- function(x, var, val_max){
  if (is.null(val_max)) {
    val_max <- max(x[[var]])
  }
  return(val_max)
}

prop_leg_values <- function(x){
  val <- seq(sqrt(min(x)), sqrt(max(x)), length.out = 4)
  val <- val * val
  return(val)
}


# adjust sizes for val_max & size_max use
adjust_size <- function(x, var, sizes, val_max, border, col, inches){
  col <- rep(col, nrow(x))
  border <- border[[1]]
  size_max <- max(sizes)

  if (inches <= size_max) {
    inches <- size_max
  } else {
    col <- c(NA, col)
    border <- c(NA, rep(border, nrow(x)))
    x <- rbind(x[1, ], x)
    x[1, var] <- val_max
    sizes <- c(inches, sizes)
  }

  x <- cbind(x, sizes, col, border)

  return(list(x = x, inches = inches))
}

plot_prop_symb <- function(symbol, x, lwd, inches) {
  switch(symbol,
         circle = {
           symbols(
             x = x[, 1],
             y = x[, 2],
             circles = x$sizes,
             bg = x$col,
             fg = x$border,
             lwd = lwd,
             add = TRUE,
             inches = inches,
             asp = 1
           )
         },
         square = {
           symbols(
             x = x[, 1],
             y = x[, 2],
             squares = x$sizes,
             bg = x$col,
             fg = x$border,
             lwd = lwd,
             add = TRUE,
             inches = inches * 2,
             asp = 1
           )
         }
  )
}

sf_to_coords <- function(x){
  cbind(st_coordinates(x), st_set_geometry(x, NULL))
}
