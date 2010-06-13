\name{HICP}
\alias{HICP}
\title{Harmonised Index of Consumer Prices (1990--2010, OECD)}

\description{
Time series data with HICP (harmonised index of consumer prices)
for 20 countries for 1990--2010 as provided by the OECD.
}

\usage{data("HICP")}

\format{
A monthly multiple \code{"zoo"} time series with \code{"yearmon"} index
from Jan 1990 to Mar 2010 for 20 countries.
}

\source{
Organisation for Economic Co-operation and Development (OECD)

\url{http://stats.oecd.org/}
}

\references{
  Wikipedia (2010). "Harmonised Index of Consumer Prices -- Wikipedia, The Free Encyclopedia."
  \url{http://en.wikipedia.org/wiki/Harmonised_Index_of_Consumer_Prices}, accessed 2010-06-10.
}

\examples{
data("HICP", package = "glogis")
if(require("lattice")) {
trellis.par.set(theme = canonical.theme(color = FALSE))
xyplot(100 * diff(log(HICP)))
}
}

\keyword{datasets}
