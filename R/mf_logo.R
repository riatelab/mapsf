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
#' @param adj adjust the position of the logo in x and y directions
#' @param resize if FALSE, the logo is displayed at its original size in pixels
#' and `cex` is not used.
#' @return No return value, a logo is displayed.
#' @export
#'
#' @examples
#' m <- mf_get_mtq()
#' mf_map(m)
#' mf_scale()
#' logo <- system.file("img", "Rlogo.png", package = "png")
#' mf_logo(logo, pos = "bottomleft", adj = c(0, 4))
#' mf_credits()
mf_logo <- function(filename, pos = "bottomright", cex = 1, adj = c(0, 0),
                    resize = TRUE) {
  test_cur_plot()
  op <- par(mar = getOption("mapsf.mar"), no.readonly = TRUE)
  on.exit(par(op))

  logo <- readimage(filename)

  if (length(pos) == 1 && pos == "interactive") {
    pos <- interleg(txt = c("logo", "Logo"))
  }

  recordGraphics(
    {
      pu <- par("usr")
      pp <- dim(logo)[2:1]
      if (resize == FALSE) {
        dev_px <- grDevices::dev.size("px")
        dev_in <- grDevices::dev.size("in")
        pp <- c(
          xinch(pp[1] * dev_in[1] / dev_px[1]),
          yinch(pp[2] * dev_in[2] / dev_px[2])
        )
      } else {
        pp <- pp * (xinch(1) / pp[1]) * cex
        pp[2] <- pp[2] * yinch(1) / xinch(1)
      }
      xy <- posinset(pos, pu, pp[1], pp[2], adj = adj)
      rasterImage(
        image   = logo,
        xleft   = xy[1],
        ybottom = xy[3],
        xright  = xy[2],
        ytop    = xy[4]
      )
    },
    list = list(pos = pos, logo = logo, cex = cex, adj = adj, resize = resize),
    env = getNamespace("mapsf")
  )
}
