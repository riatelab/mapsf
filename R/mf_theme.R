#' @title Set a theme
#' @description
#' A theme is a set of graphical parameters that are applied to maps created
#' with `mapsf`. These parameters are:
#' - figure margins and frames,
#' - background, foreground and highlight colors,
#' - default sequential and qualitative palettes,
#' - title options (position, size, banner...).
#'
#' `mapsf` offers some builtin themes. It's possible to modify an existing theme
#' or to start a theme from scratch. It is also possible to set a custom theme
#' using a list of arguments
#'
#' Themes are persistent across maps produced by `mapsf`
#' (e.g. they survive a `dev.off()` call).
#'
#' Use `mf_theme(NULL)` or `mf_theme('base')` to reset to default theme
#' settings.
#'
#' @param x name of a map theme. One of "base", "sol_dark", "sol_light",
#' "grey", "mint", "dracula", "pistachio", "rzine".
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
#' @param frame either "none", "map" or "figure"; plot a frame around the map
#' or the figure.
#' @param frame_lwd line width for the frame
#' @param frame_lty line type for the frame
#' @md
#' @note
#' Although the map theming system has been radically changed in version 1.0.0
#' of the package, you can still use the old themes by referencing them by name.
#' @return The current list of theme parameters is (invisibly) returned.
#' @export
#' @examples
#' mtq <- mf_get_mtq()
#'
#' # Choosing a theme by name:
#' mf_theme("base")
#' mf_map(mtq)
#' mf_title()
#'
#' # Specifying some values directly:
#' mf_theme(title_banner = TRUE)
#' mf_map(mtq)
#' mf_title()
#'
#' # Using a mix of the above:
#' mf_theme("sol_dark", title_tab = TRUE, title_font = 1)
#' mf_map(mtq)
#' mf_title()
#'
#' # Specifying a list with theme values:
#' theme <- list(
#'   mar = c(1, 1, 3, 1),
#'   title_tab = FALSE,
#'   title_pos = "left",
#'   title_inner = FALSE,
#'   title_line = 2,
#'   title_cex = 1.5,
#'   title_font = 2,
#'   title_banner = FALSE,
#'   frame = "figure",
#'   frame_lwd = 1,
#'   frame_lty = 1,
#'   foreground = "#fbfbfb",
#'   background = "grey75",
#'   highlight = "#0f5027",
#'   pal_quali = "Dark 3",
#'   pal_seq = "Greens"
#' )
#' mf_theme(theme)
#' mf_map(mtq, "MED", "choro")
#' mf_title()
#'
#' # Obtaining a list of parameters for the current theme:
#' current_theme <- mf_theme()
#'
#' # Use default theme:
#' mf_theme(NULL)
#' # or
#' mf_theme("base")
mf_theme <- function(x,
                     mar,
                     foreground,
                     background,
                     highlight,
                     title_tab,
                     title_pos,
                     title_inner,
                     title_line,
                     title_cex,
                     title_font,
                     title_banner,
                     frame,
                     frame_lwd,
                     frame_lty,
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
    frame        = getOption("mapsf.frame"),
    frame_lty    = getOption("mapsf.frame_lty"),
    frame_lwd    = getOption("mapsf.frame_lwd"),
    legacy       = getOption("mapsf.legacy")
  )

  # if no arg input => return param list
  argx <- as.list(match.call()[-1])
  if (length(argx) == 0) {
    return(invisible(theme))
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


  # update theme params
  # legacy themes & param
  legacy_argx <- c(
    argx$bg, argx$fg, argx$tab, argx$pos, argx$inner, argx$line,
    argx$cex, argx$font
  )
  if (!is.null(legacy_argx)) {
    message(paste0("'bg', 'fg', 'tab', 'pos', 'inner', 'line', 'cex'",
                   " and 'font' are deprecated arguments."))
    theme$legacy <- TRUE
    theme$title_banner <- TRUE
    theme$pal_quali <- "Dynamic"
    theme$pal_seq <- "Mint"
    theme$frame <- "none"
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
    if (!missing(frame)) theme$frame <- frame
    if (!missing(frame_lwd)) theme$frame_lwd <- frame_lwd
    if (!missing(frame_lty)) theme$frame_lty <- frame_lty
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
    mapsf.legacy       = theme$legacy,
    mapsf.frame        = theme$frame,
    mapsf.frame_lwd    = theme$frame_lwd,
    mapsf.frame_lty    = theme$frame_lty
  )


  return(invisible(as.list(theme)))
}
