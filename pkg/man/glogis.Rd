\name{glogis}
\alias{dglogis}
\alias{pglogis}
\alias{qglogis}
\alias{rglogis}
\alias{sglogis}

\title{The Generalized Logistic Distribution (Type I: Skew-Logitic)}

\description{
  Density, distribution function, quantile function and random
  generation for the logistic distribution with parameters
  \code{location} and \code{scale}.
}

\usage{
dglogis(x, location = 0, scale = 1, shape = 1, log = FALSE)
pglogis(q, location = 0, scale = 1, shape = 1, lower.tail = TRUE, log.p = FALSE)
qglogis(p, location = 0, scale = 1, shape = 1, lower.tail = TRUE, log.p = FALSE)
rglogis(n, location = 0, scale = 1, shape = 1)
sglogis(x, location = 0, scale = 1, shape = 1)
}

\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{location, scale, shape}{location, scale, and shape parameters (see below).}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.}
}

\value{
  \code{dglogis} gives the probability density function,
  \code{pglogis} gives the cumulative distribution function,
  \code{qglogis} gives the quantile function, and
  \code{rglogis} generates random deviates.
  \code{sglogis} gives the score function (gradient of the log-density with
    respect to the parameter vector).
}

\details{
  If \code{location}, \code{scale}, or \code{shape} are omitted, they assume the
  default values of \code{0}, \code{1}, and \code{1}, respectively.

  The generalized logistic distribution with \code{location} \eqn{= \mu}{= m},
  \code{scale} \eqn{= \sigma}{= s}, and \code{shape} \eqn{= \gamma}{= g} has distribution function
  \deqn{
    F(x) = \frac{1}{(1 + e^{-(x-\mu)/\sigma})^\gamma)}%
  }{F(x) = 1 / (1 + exp(-(x-m)/s))^g}.

  The mean is given by \code{location + (digamma(shape) - digamma(1)) * scale}, the variance by
  \code{(psigamma(shape, deriv = 1) + psigamma(1, deriv = 1)) * scale^2)} and the skewness by
  \code{(psigamma(shape, deriv = 2) - psigamma(1, deriv = 2)) / (psigamma(shape, deriv = 1) + psigamma(1, deriv = 1))^(3/2))}.

  \code{[dpq]glogis} are calculated by leveraging the \code{[dpq]logis}
  and adding the shape parameter. \code{rglogis} uses inversion.
}

\references{
  Johnson NL, Kotz S, Balakrishnan N (1995)
  \emph{Continuous Univariate Distributions}, volume 2.
  John Wiley \& Sons, New York.

  Shao Q (2002). Maximum Likelihood Estimation for Generalised Logistic Distributions.
  \emph{Communications in Statistics -- Theory and Methods}, \bold{31}(10), 1687--1700.

  Windberger T, Zeileis A (2014). Structural Breaks in Inflation Dynamics within the
    European Monetary Union. \emph{Eastern European Economics}, \bold{52}(3), 66--88.
}

\examples{
## PDF and CDF
par(mfrow = c(1, 2))
x <- -100:100/10
plot(x, dglogis(x, shape = 2), type = "l", col = 4, main = "PDF", ylab = "f(x)")
lines(x, dglogis(x, shape = 1))
lines(x, dglogis(x, shape = 0.5), col = 2)
legend("topleft", c("generalized (0, 1, 2)", "standard (0, 1, 1)",
  "generalized (0, 1, 0.5)"), lty = 1, col = c(4, 1, 2), bty = "n")
plot(x, pglogis(x, shape = 2), type = "l", col = 4, main = "CDF", ylab = "F(x)")
lines(x, pglogis(x, shape = 1))
lines(x, pglogis(x, shape = 0.5), col = 2)

## artifical empirical example
set.seed(2)
x <- rglogis(1000, -1, scale = 0.5, shape = 3)
gf <- glogisfit(x)
plot(gf)
summary(gf)
}

\keyword{distribution}
