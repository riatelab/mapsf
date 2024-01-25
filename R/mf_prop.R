#' @title Plot proportional symbols
#' @description Plot proportional symbols.
#' @eval my_params(c(
#' 'x',
#' 'var',
#' 'col',
#' 'border',
#' 'lwd',
#' 'add' ,
#' 'inches',
#' 'val_max',
#' 'lwd_max',
#' 'symbol',
#' 'leg_pos',
#' 'leg_title',
#' 'leg_title_cex',
#' 'leg_val_cex',
#' 'leg_val_rnd',
#' 'leg_frame',
#' 'leg_horiz',
#' 'leg_frame_border',
#' 'leg_bg',
#' 'leg_fg',
#' 'leg_size',
#' 'leg_adj'))
#' @importFrom graphics box
#' @keywords internal
#' @export
#' @return x is (invisibly) returned.
#' @examples
#' mtq <- mf_get_mtq()
#' mf_map(mtq)
#' mf_map(mtq, "POP", "prop")
#'
#' mf_map(mtq)
#' mf_map(
#'   x = mtq, var = "POP", type = "prop",
#'   inches = .4, symbol = "circle", val_max = 90000,
#'   col = "tomato1", border = "blue", lwd = 1,
#'   leg_pos = "right", leg_title = "Population",
#'   leg_title_cex = 1, leg_val_cex = .8, leg_val_rnd = 0,
#'   leg_frame = TRUE, add = TRUE
#' )
mf_prop <- function(x,
                    var,
                    inches = 0.3,
                    val_max,
                    lwd_max = 20,
                    symbol = "circle",
                    col = "tomato4",
                    border = getOption("mapsf.fg"),
                    lwd = .7,
                    leg_pos = mf_get_leg_pos(x),
                    leg_title = var,
                    leg_title_cex = .8,
                    leg_val_cex = .6,
                    leg_val_rnd = 0,
                    leg_frame = FALSE,
                    leg_frame_border = getOption("mapsf.fg"),
                    leg_horiz = FALSE,
                    leg_adj = c(0, 0),
                    leg_fg = getOption("mapsf.fg"),
                    leg_bg = getOption("mapsf.bg"),
                    leg_size = 1,
                    add = TRUE) {
  # default
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))

  xtype <- get_geom_type(x)
  # linestring special case
  if (xtype == "LINE") {
    xl <- x[!is.na(x[[var]]), ]
    if (!missing(val_max)) {
      maxval <- val_max
    } else {
      maxval <- max(xl[[var]])
    }
    xl$lwd <- xl[[var]] * lwd_max / maxval
    if (add == FALSE) {
      mf_init(x)
    }
    op2 <- par(lend = 1)
    mf_base(xl, lwd = xl$lwd, add = TRUE, col = col)

    val <- seq(min(xl[[var]]), max(xl[[var]]), length.out = 4)
    leg(
      type = "prop_line",
      pos = leg_pos, val = val, lwd = max(xl$lwd), col = col,
      title = leg_title, title_cex = leg_title_cex,
      val_cex = leg_val_cex, val_rnd = leg_val_rnd,
      frame = leg_frame, bg = leg_bg, fg = leg_fg, adj = leg_adj,
      frame_border = leg_frame_border, size = leg_size
    )

    par(op2)

    return(invisible(x))
  }


  # check merge and order
  dots <- create_dots(x = x, var = var)

  # default col
  mycols <- rep(col, nrow(dots))

  # Default max value
  if (missing(val_max)) {
    val_max <- max(dots[[var]])
  }

  # get sizes
  sizes <- get_size(
    var = dots[[var]], inches = inches,
    val_max = val_max, symbol = symbol
  )

  # size and values for legend, hollow circle (fixmax case)
  size_max <- max(sizes)
  val <- seq(sqrt(min(dots[[var]])), sqrt(max(dots[[var]])), length.out = 4)
  val <- val * val

  if (inches <= size_max) {
    inches <- size_max
    borders <- border
  } else {
    mycols <- c(NA, mycols)
    borders <- c(NA, rep(border, nrow(dots)))
    dots <- rbind(dots[1, ], dots)
    dots[1, var] <- val_max
    sizes <- c(inches, sizes)
  }

  # empty plot if needed
  if (add == FALSE) {
    mf_init(x)
  }

  # Plot the symbols
  plot_symbols(
    symbol = symbol, dots = dots, sizes = xinch(sizes),
    mycols = mycols, border = borders, lwd = lwd,
    inches = inches
  )

  # symbols size
  leg(
    type = "prop",
    pos = leg_pos, val = val, title = leg_title,
    symbol = symbol, inches = size_max, col = col,
    title_cex = leg_title_cex, val_cex = leg_val_cex,
    val_rnd = leg_val_rnd, horiz = leg_horiz,
    frame = leg_frame, border = border, lwd = lwd, adj = leg_adj,
    bg = leg_bg, fg = leg_fg, self_adjust = TRUE,
    mar = getOption("mapsf.mar"), frame_border = leg_frame_border,
    size = leg_size
  )

  return(invisible(x))
}
