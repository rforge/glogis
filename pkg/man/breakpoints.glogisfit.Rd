\name{breakpoints.glogisfit}
\alias{breakpoints.glogisfit}
\alias{coef.breakpoints.glogisfit}
\alias{fitted.breakpoints.glogisfit}
\alias{refit.breakpoints.glogisfit}

\title{Segmented Fitting of the Generalized Logistic Distribution}

\description{
  Fitting univariate generalized logisitc distributions (Type I: skew-logistic with
  location, scale, and shape parameters) to segments of time series data.
}

\usage{
\method{breakpoints}{glogisfit}(obj, \dots)

\method{refit}{breakpoints.glogisfit}(object, \dots)

\method{coef}{breakpoints.glogisfit}(object, log = TRUE, \dots)

\method{fitted}{breakpoints.glogisfit}(object, type = c("mean", "variance", "skewness"), \dots)
}

\arguments{
  \item{obj}{an object of class \code{\link{glogisfit}}.}
  \item{object}{an object of class \code{breakpoints.glogisfit} as returned by
    the \code{\link[strucchange]{breakpoints}} method.}
  \item{log}{logical option in \code{coef} method indicating whether scale and
    shape parameters should be reported in logs (default) or the original
    levels.}
  \item{type}{character specifying which moments of the segmented fitted distribution
    should be extracted.}
  \item{\dots}{arguments passed to methods.}
}

\details{
  To test whether sequences (typically time series) of observations follow the same
  generalized logistic distribution, the stability of the parameters can be tested.
  If there is evidence for parameter instability, breakpoints can be estimated to
  find segments with stable parameters.
  
  The methods from the \pkg{strucchange} and \pkg{fxregime} packages are leveraged.
  For testing, the generalized M-fluctuation tests from \pkg{strucchange} can directly
  be employed using \code{\link[strucchange]{gefp}}. For breakpoint estimation,
  the methods documented here provide a user interface to some internal functionality
  from the \pkg{fxregime} packages. They employ the (unexported) workhorse function
  \code{gbreakpoints} which is modeled after \code{\link[strucchange]{breakpoints}} 
  from the \pkg{strucchange} package but employing user-defined estimation methods.
}

\value{
  \code{breakpoints.glogisfit} returns an object of class \code{"breakpoints.glogisfit"} that
  inherits from \code{"gbreakpointsfull"}.
}

\seealso{\code{\link{glogisfit}}, \code{\link[fxregime]{fxregimes}}, \code{\link[strucchange]{breakpoints}}}

\examples{
## artifical data with one structural change
set.seed(1071)
x <- c(rglogis(50, -1, scale = 0.5, shape = 3), rglogis(50, 1, scale = 0.5, shape = 1))
x <- zoo(x, yearmon(seq(2000, by = 1/12, length = 100)))

## full sample estimation
gf <- glogisfit(x)

if(require("fxregime")) {

## structural change testing
gf_scus <- gefp(gf, fit = NULL)
plot(gf_scus, aggregate = FALSE)
plot(gf_scus, functional = meanL2BB)
sctest(gf_scus)
sctest(gf_scus, functional = meanL2BB)

## breakpoint estimation
gf_bp <- breakpoints(gf)
plot(gf_bp)
summary(gf_bp)
breakdates(gf_bp)
coef(gf_bp)

## fitted model
plot(x)
lines(gf_bp)
lines(fitted(gf_bp, type = "mean"), col = 4)
}
}

\keyword{regression}
