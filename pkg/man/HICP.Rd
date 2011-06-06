\name{HICP}
\alias{HICP}
\alias{hicps}

\encoding{latin1}

\title{Harmonised Index of Consumer Prices (1990--2010, OECD)}

\description{
Time series data with HICP (Harmonised Index of Consumer Prices)
for 21 countries (plus EU) for 1990--2010 as provided by the OECD; and
corresponding seasonally adjusted inflation ratios.
}

\usage{
  data("HICP")

  data("hicps")  
}

\format{
Monthly multiple \code{"zooreg"} time series with \code{"yearmon"} index
from Jan 1990 (\code{HICP}) or Feb 1990 (\code{hicps}) to Dec 2010 for
21 countries (plus EU).
}

\details{
\code{HICP} contains the raw unadjusted Harmonised Index of Consumer Prices
as provided by the OECD from which unadjusted inflation rates can be easily
computed (see examples).

As the different countries have rather different seasonal patterns which
vary over time (especially in the 2000s), they will typically require seasonal
adjustment before modeling. Hence, a seasonally adjusted version of the
inflation rate series is provided as \code{hicps}, where X-12-ARIMA (version 0.3)
has been employed for adjusted. An alternative seasonal adjustment can be easily
computed use \code{\link{stl}} (see examples).
}

\source{
Organisation for Economic Co-operation and Development (OECD)

\url{http://stats.oecd.org/}
}

\references{
  Wikipedia (2010). "Harmonised Index of Consumer Prices -- Wikipedia, The Free Encyclopedia."
  \url{http://en.wikipedia.org/wiki/Harmonised_Index_of_Consumer_Prices}, accessed 2010-06-10.

  Windberger T, Zeileis A (2011). Structural Breaks in Inflation Dynamics within
  the European Monetary Union. Working Paper 2011-12. Working Papers in Economics and Statistics,
  Research Platform Empirical and Experimental Economics, Universität Innsbruck.
  \url{http://EconPapers.RePEc.org/RePEc:inn:wpaper:2011-12}
}

\examples{
## price series
data("HICP", package = "glogis")

## corresponding raw unadjusted inflation rates (in percent)
hicp <- 100 * diff(log(HICP))

## seasonal adjustment of inflation rates (via STL)
hicps1 <- do.call("merge", lapply(1:ncol(hicp), function(i) {
  z <- na.omit(hicp[,i])
  coredata(z) <- coredata(as.ts(z) - stl(as.ts(z), s.window = 13)$time.series[, "seasonal"])
  z
}))
colnames(hicps1) <- colnames(hicp)

## load X-12-ARIMA adjusted inflation rates
data("hicps", package = "glogis")

## compare graphically for one country (Austria)
plot(hicp[, "Austria"], lwd = 2, col = "lightgray")
lines(hicps1[, "Austria"], col = "red")
lines(hicps[, "Austria"], col = "blue")
legend("topleft", c("unadjusted", "STL", "X-12-ARIMA"), lty = c(1, 1, 1),
  col = c("lightgray", "red", "blue"), bty = "n")

## compare graphically across all countries (via lattice)
if(require("lattice")) {
trellis.par.set(theme = canonical.theme(color = FALSE))
xyplot(merge(hicp, hicps1, hicps), 
  screen = names(hicp)[rep(1:ncol(hicp), 3)],
  col = c("lightgray", "red", "blue")[rep(1:3, each = ncol(hicp))],
  lwd = c(2, 1, 1)[rep(1:3, each = ncol(hicp))])
}


}

\keyword{datasets}
