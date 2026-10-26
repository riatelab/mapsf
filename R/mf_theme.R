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
#' Current theme parameters are set
#' in `mapsf` options and nammed according to the following convention:
#' "mapsf.mf_theme_arg_name".
#' Use `getOption()` to return the value of a specific argument of
#' the current theme (see examples).
#'
#' Use `mf_theme(NULL)` or `mf_theme('base')` to reset to default theme
#' settings.
#'
#' @eval paste0("@param x name of a map theme. One of '",
#' paste0(get_themes_names(), collapse= "', '" ),
#' "'. The default theme is 'base'.")
#' @param mar a numeral vector of the form c(bottom, left, top, right)
#' which gives the margin size specified in number of lines
#' @param title_pos title position, one of 'left', 'center', 'right'
#' @param title_tab if TRUE the title is displayed as a 'tab'
#' @param title_cex size of the title
#' @param title_font font of the title
#' @param title_line number of margin lines used for the title
#' @param title_inner if TRUE the title is displayed inside the plot area;
#' if FALSE the title is displayed in the top margin
#' @param title_banner if TRUE the title is displayed as a banner
#' @param foreground foreground color, hex code or color name given by [colors].
#' @param background background color, hex code or color name given by [colors].
#' @param highlight highlight color, hex code or color name given by [colors].
#' @param pal_quali default qualitative color palette (name or function).
#' Palette names can be obtained with [hcl.pals].
#' @param pal_seq default sequential color palette (name or function).
#' Palette names can be obtained with [hcl.pals].
#' @param frame either "none", "map" or "figure"; plot a frame around the map
#' or the figure.
#' @param frame_lwd line width for the frame
#' @param frame_lty line type for the frame
#' @return `mf_theme` (invisibly) returns the list of current theme parameters.
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
#' # Obtaining individual parameters for the current theme:
#' getOption("mapsf.highlight")
#' getOption("mapsf.pal_seq")
#'
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
                     pal_seq) {
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
    frame_lwd    = getOption("mapsf.frame_lwd")
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
      argx <- c(
        "mar", "foreground", "background", "highlight",
        "title_tab", "title_pos", "title_inner",
        "title_line", "title_cex", "title_font",
        "title_banner", "frame", "frame_lwd",
        "frame_lty", "pal_quali", "pal_seq"
      )
      if (all(names(x) %in% argx)) {
        theme <- x[names(x) %in% argx]
      }
    } else {
      if (!x %in% names(.gmapsf$themes)) {
        stop("x is not a theme name.", call. = FALSE)
      } else {
        theme <- .gmapsf$themes[[x]]
      }
    }
  }


  # update theme params
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
    mapsf.frame        = theme$frame,
    mapsf.frame_lwd    = theme$frame_lwd,
    mapsf.frame_lty    = theme$frame_lty
  )
  return(invisible(as.list(theme)))
}
