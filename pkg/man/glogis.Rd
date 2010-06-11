\name{glogis}
\alias{gglogis}
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
sglogis(n, location = 0, scale = 1, shape = 1)
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

  \code{[dpq]glogis} are calculated by leveraging the \code{[dpq]logis}
  and adding the shape parameter. \code{rglogis} uses inversion.
}

\references{
  Johnson NL, Kotz S, Balakrishnan N (1995)
  \emph{Continuous Univariate Distributions}, volume 2.
  John Wiley \& Sons, New York.
}

\examples{
x <- rglogis(1000, -1, scale = 0.5, shape = 3)
gf <- glogisfit(x)
plot(gf)
summary(gf)
}

\keyword{distribution}
