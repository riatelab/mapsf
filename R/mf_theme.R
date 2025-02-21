#' @title Set a theme
#' @description This function set a map theme.
#' The parameters set by this function are the figure margins, background and
#' foreground colors and some \link{mf_title} options.
#' Use \code{mf_theme(NULL)} or \code{mf_theme('default')} to reset to default
#' theme settings.
#'
#' @param x name of a map theme. One of "default", "brutal", "ink",
#' "dark", "agolalight", "candy", "darkula", "iceberg", "green", "nevermind",
#' "jsk", "barcelona".
#' @param mar margins
#' @param title_pos title position, one of 'left', 'center', 'right'
#' @param title_tab if TRUE the title is displayed as a 'tab'
#' @param title_cex cex of the title
#' @param title_font font of the title
#' @param title_line number of lines used for the title
#' @param title_inner if TRUE the title is displayed inside the plot area.
#' @param title_banner if TRUE the title is displayed as a banner
#' @param foreground foreground color
#' @param background background color
#' @param highlight highlight color
#' @param pal_quali default qualitative color palette (name or function)
#' @param pal_seq default sequential color palettte (name or function)
#' @param ... other argument, ignored
#'
#' @details
#' It is also possible to set a custom theme using a list of arguments
#' (see Examples).
#' \code{mf_theme()} returns the current theme settings.
#' @return The (invisible) list of theme parameters is returned.
#' @export
#' @examples
#' mtq <- mf_get_mtq()
#'
#' # Choosing a theme by name:
#' mf_theme("default")
#' mf_map(mtq)
#' mf_title()
#'
#' # Specifying some values directly:
#' mf_theme(bg = "darkslategrey", fg = "lightgrey")
#' mf_map(mtq)
#' mf_title()
#'
#' # Using a mix of the above:
#' mf_theme("brutal", fg = "lightgreen", pos = "center", font = 2, tab = FALSE)
#' mf_map(mtq)
#' mf_title()
#'
#' # Specifying a list with theme values:
#' theme <- mf_theme("default")
#' theme$mar <- c(1, 1, 3, 1)
#' theme$line <- 2
#' theme$cex <- 1.5
#' mf_theme(theme)
#' mf_map(mtq)
#' mf_title()
#'
#' # or
#' theme <- list(
#'   bg = "green",
#'   fg = "red",
#'   mar = c(2, 2, 2, 2),
#'   tab = TRUE,
#'   pos = "center",
#'   inner = TRUE,
#'   line = 2,
#'   cex = 1.5,
#'   font = 3
#' )
#' mf_theme(theme)
#' mf_map(mtq)
#' mf_title()
#'
#' # Obtaining a list of parameters for the current theme:
#' mf_theme()
#'
#' # Removing the current theme:
#' mf_theme(NULL)
#' # or
#' mf_theme("default")
mf_theme <- function(x,
                     mar,
                     title_tab,
                     title_pos,
                     title_inner,
                     title_line,
                     title_cex,
                     title_font,
                     title_banner,
                     foreground,
                     background,
                     highlight,
                     pal_quali,
                     pal_seq, ...) {
  # current theme
  theme <- list(
    mar          = getOption("mapsf.mar"),
    title_tab    = getOption("mapsf.title_tab"),
    title_pos    = getOption("mapsf.title_pos"),
    title_inner  = getOption("mapsf.title_inner"),
    title_line   = getOption("mapsf.title_line"),
    title_cex    = getOption("mapsf.title_cex"),
    title_font   = getOption("mapsf.title_font"),
    title_banner = getOption("mapsf.title_banner"),
    foreground   = getOption("mapsf.foreground"),
    background   = getOption("mapsf.background"),
    highlight    = getOption("mapsf.highlight"),
    pal_quali    = getOption("mapsf.pal_quali"),
    pal_seq      = getOption("mapsf.pal_seq"),
    legacy       = getOption("mapsf.legacy")
  )

  # if no arg input => return param list
  argx <- as.list(match.call()[-1])
  if (length(argx) == 0) {
    return(theme)
  }


  # input a theme name
  if (!missing(x)) {
    # if is.null(x) => set base theme
    if (is.null(x)) {
      x <- "base"
    }
    # if x is a list of args
    if (is.list(x)) {
      argx <- theme <- x
    } else {
      if (!x %in% names(.gmapsf$themes)) {
        stop("x is not a theme name.", call. = FALSE)
      } else {
        theme <- .gmapsf$themes[[x]]
      }
    }
  }


  # modify theme param
  legacy_argx <- c(
    argx$bg, argx$fg, argx$tab, argx$pos,
    argx$inner, argx$line, argx$cex, argx$font
  )
  if (!is.null(legacy_argx)) {
    theme$legacy <- TRUE
    theme$title_banner <- TRUE
    theme$pal_quali <- "Dynamic"
    theme$pal_seq <- "Mint"
    if (!is.null(argx$bg)) theme$background <- argx$bg
    if (!is.null(argx$fg)) theme$foreground <- argx$fg
    if (!is.null(argx$fg)) theme$highlight <- argx$fg
    if (!is.null(argx$tab)) theme$title_tab <- argx$tab
    if (!is.null(argx$pos)) theme$title_pos <- argx$pos
    if (!is.null(argx$inner)) theme$title_inner <- argx$inner
    if (!is.null(argx$line)) theme$title_line <- argx$line
    if (!is.null(argx$cex)) theme$title_cex <- argx$cex
    if (!is.null(argx$font)) theme$title_font <- argx$font
  } else {
    if (!missing(title_tab)) theme$title_tab <- title_tab
    if (!missing(title_pos)) theme$title_pos <- title_pos
    if (!missing(title_inner)) theme$title_inner <- title_inner
    if (!missing(title_line)) theme$title_line <- title_line
    if (!missing(title_cex)) theme$title_cex <- title_cex
    if (!missing(title_font)) theme$title_font <- title_font
    if (!missing(title_banner)) theme$title_banner <- title_banner
    if (!missing(foreground)) theme$foreground <- foreground
    if (!missing(background)) theme$background <- background
    if (!missing(highlight)) theme$highlight <- highlight
    if (!missing(pal_quali)) theme$pal_quali <- pal_quali
    if (!missing(pal_seq)) theme$pal_seq <- pal_seq
  }
  if (!missing(mar)) theme$mar <- mar


  # set theme options
  options(
    mapsf.mar          = theme$mar,
    mapsf.title_tab    = theme$title_tab,
    mapsf.title_pos    = theme$title_pos,
    mapsf.title_inner  = theme$title_inner,
    mapsf.title_line   = theme$title_line,
    mapsf.title_cex    = theme$title_cex,
    mapsf.title_font   = theme$title_font,
    mapsf.title_banner = theme$title_banner,
    mapsf.foreground   = theme$foreground,
    mapsf.background   = theme$background,
    mapsf.highlight    = theme$highlight,
    mapsf.pal_quali    = theme$pal_quali,
    mapsf.pal_seq      = theme$pal_seq,
    mapsf.legacy       = theme$legacy
  )


  return(invisible(as.list(theme)))
}
