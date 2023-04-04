#' @title Plot proportional symbols using typology coloration
#' @description Plot proportional symbols with colors based on qualitative data.
#' @eval my_params(c(
#' 'x',
#' 'var',
#' 'border',
#' 'lwd',
#' 'add' , 'lwd_max',
#' 'inches', 'val_max', 'symbol', 'col_na', 'pal', 'alpha', 'leg_val_rnd',
#' 'leg_pos2', 'leg_title', 'leg_title_cex', 'leg_val_cex', 'val_order',
#' 'leg_no_data', 'leg_frame'))
#'
#' @importFrom methods is
#' @keywords internal
#' @export
#' @return x is (invisibly) returned.
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_prop_typo(mtq, c("POP", "STATUS"))
#'
#' mtq[6, "STATUS"] <- NA
#' mf_map(mtq)
#' mf_prop_typo(
#'   x = mtq, var = c("POP", "STATUS"), inches = .35, border = "tomato4",
#'   val_max = 90000, symbol = "circle", col_na = "grey", pal = "Dynamic",
#'   lwd = 2,
#'   leg_pos = c("bottomright", "bottomleft"),
#'   leg_title = c("Population", "Municipality\nstatus"),
#'   leg_title_cex = c(0.9, 0.9),
#'   leg_val_cex = c(.7, .7),
#'   val_order = c("Prefecture", "Sub-prefecture", "Simple municipality"),
#'   leg_no_data = "No dada",
#'   leg_frame = c(TRUE, TRUE),
#'   add = TRUE
#' )
mf_prop_typo <- function(x, var,
                         inches = 0.3,
                         val_max,
                         symbol = "circle",
                         pal = "Dynamic",
                         alpha = 1,
                         val_order,
                         border,
                         lwd = .7,
                         lwd_max = 15,
                         col_na = "white",
                         leg_pos = mf_get_leg_pos(x, 2),
                         leg_title = var,
                         leg_title_cex = c(.8, .8),
                         leg_val_cex = c(.6, .6),
                         leg_val_rnd = c(0),
                         leg_no_data = "No data",
                         leg_frame = c(FALSE, FALSE),
                         add = TRUE) {
  # default
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  lend <- par("lend")

  on.exit(par(op))
  bg <- getOption("mapsf.bg")
  fg <- getOption("mapsf.fg")
  if (missing(border)) border <- fg

  var2 <- var[2]
  var1 <- var[1]


  xtype <- get_geom_type(x)
  # linestring special case
  if (xtype == "LINE") {
    xl <- x[!is.na(x[[var1]]), ]
    if (!missing(val_max)) {
      maxval <- val_max
    } else {
      maxval <- max(xl[[var1]])
    }
    xl$lwd <- xl[[var1]] * lwd_max / maxval
    if (add == FALSE) {
      mf_init(x)
    }
    xl <- xl[xl[[var1]] != 0, ]
    # turn to positive values
    xl[[var1]] <- abs(xl[[var1]])
    # Order the dots
    xl <- xl[order(xl[[var1]], decreasing = TRUE), ]

    val_order <- get_modalities(
      x = xl[[var2]],
      val_order = val_order
    )
    # get color list and association
    pal <- get_the_pal(pal = pal, nbreaks = length(val_order), alpha = alpha)
    # get color vector
    mycols <- get_col_typo(
      x = xl[[var2]], pal = pal,
      val_order = val_order
    )

    no_data <- FALSE
    if (max(is.na(mycols)) == 1) {
      no_data <- TRUE
    }
    mycols[is.na(mycols)] <- col_na
    par(lend = 1)
    mf_base(xl, lwd = xl$lwd, add = TRUE, col = mycols)
    val <- seq(min(xl[[var1]]), max(xl[[var1]]), length.out = 4)
    leg_pos <- split_leg(leg_pos)
    mf_legend_pl(
      pos = leg_pos[[1]], val = val, lwd = max(xl$lwd),
      col = "grey20",
      title = leg_title[1], title_cex = leg_title_cex[1],
      val_cex = leg_val_cex[1], val_rnd = leg_val_rnd,
      frame = leg_frame[1], bg = bg, fg = fg
    )
    mf_legend_t(
      pos = leg_pos[[2]], val = val_order, title = leg_title[2],
      title_cex = leg_title_cex[2], val_cex = leg_val_cex[2],
      col_na = col_na, no_data = no_data, no_data_txt = leg_no_data,
      frame = leg_frame[2], pal = pal, bg = bg, fg = fg
    )
    par(lend = lend)
    return(invisible(x))
  }

  # check merge and order
  dots <- create_dots(x = x, var = var1)

  # get modalities
  val_order <- get_modalities(
    x = dots[[var2]],
    val_order = val_order
  )
  # get color list and association
  pal <- get_the_pal(pal = pal, nbreaks = length(val_order), alpha = alpha)
  # get color vector
  mycols <- get_col_typo(
    x = dots[[var2]], pal = pal,
    val_order = val_order
  )

  no_data <- FALSE
  if (max(is.na(mycols)) == 1) {
    no_data <- TRUE
  }
  mycols[is.na(mycols)] <- col_na

  # Default max value
  if (missing(val_max)) {
    val_max <- max(dots[[var1]])
  }

  # get sizes
  sizes <- get_size(
    var = dots[[var1]], inches = inches,
    val_max = val_max, symbol = symbol
  )

  # size and values for legend, hollow circle (fixmax case)
  size_max <- max(sizes)
  val <- seq(sqrt(min(dots[[var1]])), sqrt(max(dots[[var1]])), length.out = 4)
  val <- val * val
  if (inches <= size_max) {
    inches <- size_max
    borders <- border
  } else {
    mycols <- c(NA, mycols)
    borders <- c(NA, rep(border, nrow(dots)))
    dots <- rbind(dots[1, ], dots)
    dots[1, var1] <- val_max
    sizes <- c(inches, sizes)
  }

  # empty plot if needed
  if (add == FALSE) {
    mf_init(x)
  }

  # Plot the symbols
  plot_symbols(
    symbol = symbol, dots = dots, sizes = mxinch(sizes),
    mycols = mycols, border = borders, lwd = lwd,
    inches = inches
  )

  leg_pos <- split_leg(leg_pos)
  # symbols size
  mf_legend_p(
    pos = leg_pos[[1]], val = val, title = leg_title[1],
    symbol = symbol, inches = size_max, col = "grey80",
    title_cex = leg_title_cex[1], val_cex = leg_val_cex[1],
    val_rnd = leg_val_rnd,
    frame = leg_frame[1], border = border, lwd = lwd, bg = bg, fg = fg,
    self_adjust = TRUE
  )

  mf_legend_t(
    pos = leg_pos[[2]], val = val_order, title = leg_title[2],
    title_cex = leg_title_cex[2], val_cex = leg_val_cex[2],
    col_na = col_na, no_data = no_data, no_data_txt = leg_no_data,
    frame = leg_frame[2], pal = pal, bg = bg, fg = fg
  )

  return(invisible(x))
}
