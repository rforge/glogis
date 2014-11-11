\name{glogisfit}
\alias{glogisfit}
\alias{glogisfit.default}
\alias{glogisfit.formula}

\alias{bread.glogisfit}
\alias{coef.glogisfit}
\alias{estfun.glogisfit}
\alias{hist.glogisfit}
\alias{lines.glogisfit}
\alias{logLik.glogisfit}
\alias{plot.glogisfit}
\alias{print.glogisfit}
\alias{print.summary.glogisfit}
\alias{residuals.glogisfit}
\alias{summary.glogisfit}
\alias{vcov.glogisfit}

\title{Fitting the Generalized Logistic Distribution}

\description{
  Fit a univariate generalized logisitc distribution (Type I: skew-logistic with
  location, scale, and shape parameters) to a sample of observations.
}

\usage{
glogisfit(x, \dots)
\method{glogisfit}{default}(x, weights = NULL, start = NULL, fixed = c(NA, NA, NA),
  method = "BFGS", hessian = TRUE, \dots)
\method{glogisfit}{formula}(formula, data, subset, na.action, weights, x = TRUE, \dots)

\method{plot}{glogisfit}(x, main = "", xlab = NULL, fill = "lightgray",
  col = "blue", lwd = 1, lty = 1, xlim = NULL, ylim = NULL,
  legend = "topright", moments = FALSE, \dots)

\method{summary}{glogisfit}(object, log = TRUE, breaks = NULL, \dots)
\method{coef}{glogisfit}(object, log = TRUE, \dots)
\method{vcov}{glogisfit}(object, log = TRUE, \dots)
}

\arguments{
  \item{x}{a vector of observation (may be a \code{\link[stats]{ts}} or \code{\link[zoo]{zoo}}
    time series).}
  \item{weights}{optional numeric vector of weights.}
  \item{start}{optional vector of starting values. The parametrization has to be
    in terms of \code{location}, \code{log(scale)}, \code{log(shape)} where the
    original parameters (without logs) are as in \code{\link{dglogis}}. Default
    is to use \code{c(0, 0, 0)} (i.e., standard logistic). For details see below.}
  \item{fixed}{specification of fixed parameters (see description of \code{start}).
    \code{NA} signals that the corresponding parameter should be estimated.
    A standard logistic distribution could thus be fitted via \code{fixed = c(NA, NA, 0)}.}
  \item{method}{character string specifying optimization method, see \code{\link[stats]{optim}}
    for the available options. Further options can be passed to \code{optim} through
    \code{...}.}
  \item{hessian}{logical. Should the Hessian be used to compute the variance/covariance
    matrix? If \code{FALSE}, no covariances or standard errors will be available in
    subsequent computations.}
  \item{formula}{symbolic description of the model, currently only \code{x ~ 1} is
    supported.}
  \item{data, subset, na.action}{arguments controlling formula processing
    via \code{\link[stats]{model.frame}}.}
  \item{main, xlab, fill, col, lwd, lty, xlim, ylim}{standard graphical parameters, see
    \code{\link[graphics]{plot}} and \code{\link[graphics]{par}}.}
  \item{legend}{logical or character specification where to place a legend.
    \code{legend = FALSE} suppresses the legend.
    See \code{\link[graphics]{legend}} for the character specification.}
  \item{moments}{logical. If a legend is produced, it can either show the parameter
    estimates (\code{moments = FALSE}, default) or the implied moments of the
    distribution.}
  \item{object}{a fitted \code{glogisfit} object.}
  \item{log}{logical option in some extractor methods indicating whether scale and
    shape parameters should be reported in logs (default) or the original
    levels.}
  \item{breaks}{interval breaks for the chi-squared goodness-of-fit test.
    Either a numeric vector of two or more cutpoints or a single number (greater than or
    equal to 2) giving the number of intervals.}
  \item{\dots}{arguments passed to methods.}
}

\details{
  \code{glogisfit} estimates the generalized logistic distribution (Type I: skew-logistic)
  as given by \code{\link{dglogis}}. Optimization is performed numerically by
  \code{\link[stats]{optim}} using analytical gradients. For obtaining numerically more
  stable results the scale and shape parameters are specified in logs. Starting values
  are chosen as \code{c(0, 0, 0)}, i.e., corresponding to a standard (symmetric) logistic
  distribution. If these fail, better starting values are obtained by running a Nelder-Mead
  optimization on the original problem (without logs) first.  
  
  A large list of standard extractor methods is supplied to conveniently compute
  with the fitted objects, including methods to the generic functions
  \code{\link[base]{print}}, \code{\link[base]{summary}}, \code{\link[graphics]{plot}}
  (reusing \code{\link[graphics]{hist}} and \code{\link[graphics]{lines}}), \code{\link[stats]{coef}}, 
  \code{\link[stats]{vcov}}, \code{\link[stats]{logLik}},  \code{\link[stats]{residuals}},
  and \code{\link[sandwich]{estfun}} and
  \code{\link[sandwich]{bread}} (from the \pkg{sandwich} package).  

  The methods for \code{coef}, \code{vcov}, \code{summary}, and \code{bread} report computations
  pertaining to the scale/shape parameters in logs by default, but allow for switching back to
  the original levels (employing the delta method).
  
  Visualization employs a histogramm of the original data along with lines for the estimated
  density.
  
  Further structural change methods for \code{"glogisfit"} objects are described in
  \code{\link{breakpoints.glogisfit}}.
}

\value{
  \code{glogisfit} returns an object of class \code{"glogisfit"}, i.e., a list with components as follows.
  \item{coefficients}{estimated parameters from the model (with scale/shape in logs, if included),}
  \item{vcov}{associated estimated covariance matrix,}
  \item{loglik}{log-likelihood of the fitted model,}
  \item{df}{number of estimated parameters,}
  \item{n}{number of observations,}
  \item{nobs}{number of observations with non-zero weights,}
  \item{weights}{the weights used (if any),}
  \item{optim}{output from the \code{optim} call for maximizing the log-likelihood,}
  \item{method}{the method argument passed to the \code{optim} call,}
  \item{parameters}{the full set of model parameters (location/scale/shape), including
    estimated and fixed parameters, all in original levels (without logs),}
  \item{moments}{associated mean/variance/skewness,}
  \item{start}{the starting values for the parameters passed to the \code{optim} call,}
  \item{fixed}{the original specification of fixed parameters,}
  \item{call}{the original function call,}
  \item{x}{the original data,}
  \item{converged}{logical indicating successful convergence of \code{optim},}
  \item{terms}{the terms objects for the model (if the \code{formula} method was used).}
}

\references{
  Shao Q (2002). Maximum Likelihood Estimation for Generalised Logistic Distributions.
  \emph{Communications in Statistics -- Theory and Methods}, \bold{31}(10), 1687--1700.

  Windberger T, Zeileis A (2014). Structural Breaks in Inflation Dynamics within the
    European Monetary Union. \emph{Eastern European Economics}, \bold{52}(3), 66--88.
}

\seealso{\code{\link{dglogis}}, \code{\link{dlogis}}, \code{\link{breakpoints.glogisfit}}}

\examples{
## simple artificial example
set.seed(2)
x <- rglogis(1000, -1, scale = 0.5, shape = 3)
gf <- glogisfit(x)
plot(gf)
summary(gf)

## query parameters and associated moments
coef(gf)
coef(gf, log = FALSE)
gf$parameters
gf$moments
}

\keyword{regression}
