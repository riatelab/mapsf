% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mf_inset.R
\name{mf_inset_on}
\alias{mf_inset_on}
\alias{mf_inset_off}
\title{Plot an inset}
\usage{
mf_inset_on(x, pos = "topright", cex = 0.2, fig)

mf_inset_off()
}
\arguments{
\item{x}{an sf object, or "worldmap" to use with \link{mf_worldmap}.}

\item{pos}{position, one of "bottomleft", "left", "topleft", "top", "bottom",
"bottomright", "right", "topright"}

\item{cex}{share of the map width occupied by the inset}

\item{fig}{coordinates of the inset region (in NDC, see in ?par())}
}
\value{
No return value, an inset is initiated or closed.
}
\description{
This function is used to add an inset map to the current map.
}
\details{
If x is used (with pos and cex), the width/height ratio of the inset will
match the width/height ratio of x bounding box.\cr
If fig is used, coordinates (xmin, xmax, ymin, ymax) are expressed as
fractions of the mapping space (i.e. excluding margins).\cr
If map layers have to be plotted after the inset (i.e after mf_inset_off()),
please use add = TRUE.\cr
It is not possible to plot an inset within an inset.\cr
It is possible to plot anything (base plots) within the inset, not only map
layers.
}
\note{
This function does not work when mfrow is used in par().
}
\examples{
mtq <- mf_get_mtq()
mf_map(mtq)
mf_inset_on(x = mtq[1, ], cex = .2)
mf_map(mtq[1, ])
mf_inset_off()

mf_map(mtq)
mf_inset_on(x = "worldmap", pos = "bottomleft")
mf_worldmap(x = mtq)
mf_inset_off()

mf_map(mtq)
mf_inset_on(fig = c(0, 0.25, 0, 0.25))
mf_map(x = mtq)
mf_inset_off()
}
