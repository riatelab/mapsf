#' @title Plot a logo on a map
#'
#' @description
#' The logo can be a PNG or JPG/JPEG file.
#'
#' @param filename filename of the logo image, PNG or JPG/JPEG format.
#' @param pos position of the logo, one of 'topleft', 'top','topright',
#' 'right', 'bottomright', 'bottom', 'bottomleft', 'left' or a vector of two
#' coordinates in map units (c(x, y)). Use 'interactive' to choose the
#' legend position by clicking on the map.
#' @param cex amount by which the logo width should be magnified
#' or reduced relative to the default
#' @param adj adjust the postion of the logo in x and y directions
#'
#' @return No return value, a background image is displayed.
#' @export
#'
#' @examples
#' m <- mf_get_mtq()
#' mf_map(m)
#' mf_scale()
#' logo <- system.file("img", "Rlogo.png", package = "png")
#' mf_logo(logo, pos = "bottomleft", adj = c(0, 4))
#' mf_credits()
mf_logo <- function(filename, pos = "bottomright", cex = 1, adj = c(0, 0)) {
  test_cur_plot()
  logo <- readimage(filename)
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))

  if (length(pos) == 1 && pos == "interactive") {
    pos <- mapsf:::interleg(txt = c("logo", "Logo"))
  }

  recordGraphics(
    {
      pu <- par("usr")
      pp <- dim(logo)[2:1]
      pp <- pp * (xinch(1) / pp[1]) * cex
      xy <- posinset(pos, pu, pp[1], pp[2], adj = adj)

      rasterImage(
        image   = logo,
        xleft   = xy[1],
        ybottom = xy[3],
        xright  = xy[2],
        ytop    = xy[4]
      )
    },
    list = list(pos = pos, logo = logo, cex = cex, adj = adj),
    env = getNamespace("mapsf")
  )
}
