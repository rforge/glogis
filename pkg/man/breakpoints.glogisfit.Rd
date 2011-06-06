\name{breakpoints.glogisfit}
\alias{breakpoints.glogisfit}
\alias{coef.breakpoints.glogisfit}
\alias{fitted.breakpoints.glogisfit}
\alias{refit.breakpoints.glogisfit}
\alias{index.breakpoints.glogisfit}

\alias{confint.breakpoints.glogisfit}
\alias{breakdates.confint.breakpoints.glogisfit}
\alias{print.confint.breakpoints.glogisfit}
\alias{lines.confint.breakpoints.glogisfit}

\encoding{latin1}

\title{Segmented Fitting of the Generalized Logistic Distribution}

\description{
  Fitting univariate generalized logisitc distributions (Type I: skew-logistic with
  location, scale, and shape parameters) to segments of time series data.
}

\usage{
\method{breakpoints}{glogisfit}(obj, h = 0.15, breaks = NULL, ic = "LWZ",
  hpc = "none", \dots)

\method{refit}{breakpoints.glogisfit}(object, \dots)

\method{coef}{breakpoints.glogisfit}(object, log = TRUE, \dots)

\method{fitted}{breakpoints.glogisfit}(object, type = c("mean", "variance", "skewness"), \dots)

\method{confint}{breakpoints.glogisfit}(object, parm = NULL, level = 0.95, breaks = NULL, 
  meat. = NULL, \dots)
}

\arguments{
  \item{obj}{an object of class \code{\link{glogisfit}}.}
  \item{h}{numeric. Minimal segment size either given as fraction relative to the
    sample size or as an integer giving the minimal number of observations
    in each segment.}
  \item{breaks}{integer specifying the maximal number of breaks to be calculated.
    By default the maximal number allowed by \code{h} is used.}
  \item{ic}{character specifying the default information criterion that should
    be employed for selecting the number of breakpoints. Default is \code{"LWZ"}
    (Liu-Wu-Zidek criterion, a modified BIC). Instead the classic \code{"BIC"}
    can be used.}  
  \item{hpc}{a character specifying the high performance computing support.
    Default is \code{"none"}, can be set to \code{"foreach"}.}
  \item{object}{an object of class \code{breakpoints.glogisfit} as returned by
    the \code{\link[strucchange]{breakpoints}} method.}
  \item{log}{logical option in \code{coef} method indicating whether scale and
    shape parameters should be reported in logs (default) or the original
    levels.}
  \item{type}{character specifying which moments of the segmented fitted distribution
    should be extracted.}
  \item{parm}{integer. Either \code{parm} or \code{breaks} may be set, see below.}
  \item{level}{numeric. The confidence level to be used.}
  \item{meat.}{function. A function for extracting the meat of a sandwich estimator
    from a fitted object. By default, the inverse of \code{\link[sandwich]{bread}}
    is used, i.e., a correctly specified model is assumed.}
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

  Optional support for high performance computing is available in the \code{breakpoints}
  method, currently using \code{\link[foreach]{foreach}} for the dynamic programming algorithm.
  If \code{hpc = "foreach"} is to be used, a parallel backend should be registered
  before. See \code{\link[foreach]{foreach}} for more information.
}

\value{
  \code{breakpoints.glogisfit} returns an object of class \code{"breakpoints.glogisfit"} that
  inherits from \code{"gbreakpointsfull"}.
}

\references{
  Windberger T, Zeileis A (2011). Structural Breaks in Inflation Dynamics within
  the European Monetary Union. Working Paper 2011-12. Working Papers in Economics and Statistics,
  Research Platform Empirical and Experimental Economics, Universität Innsbruck.
  \url{http://EconPapers.RePEc.org/RePEc:inn:wpaper:2011-12}

  Zeileis A, Shah A, Patnaik I (2010). Testing, Monitoring, and Dating Structural
  Changes in Exchange Rate Regimes. \emph{Computational Statistics and Data Analysis},
  \bold{54}(6), 1696--1706. \url{http://dx.doi.org/10.1016/j.csda.2009.12.005}.
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
confint(gf_bp)

## fitted model
plot(x)
lines(gf_bp)
lines(fitted(gf_bp, type = "mean"), col = 4)
lines(confint(gf_bp))
}
}

\keyword{regression}
