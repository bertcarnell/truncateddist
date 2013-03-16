\name{tgamma}
\alias{tgamma}
\alias{rtgamma}
\alias{dtgamma}
\alias{ptgamma}
\alias{qtgamma}
\title{Truncated Gamma Functions}
\description{
  pdf, cdf, inverse cdf, and random deviates of the truncated gamma distribution.
}
\usage{
rtgamma(n, shape, scale=1, a=-Inf, b=Inf)
dtgamma(x, shape, scale=1, a=-Inf, b=Inf)
ptgamma(x, shape, scale=1, a=-Inf, b=Inf)
qtgamma(p, shape, scale=1, a=-Inf, b=Inf)
}
\arguments{
  \item{x,q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If length(n) > 1, the length is taken to be the number required.}
  \item{shape}{vector of shape parameters.}
  \item{scale}{vector of scale parameters.}
  \item{a}{vector of lower truncation limits}
  \item{b}{vector of upper truncation limits}
}
\value{
  dtgamma gives the density, ptgamma gives the distribution function, qtgamma gives the quantile function, and rtgamma generates random deviates.
}
\author{Rob Carnell}
\examples{
  rtgamma(5, 1, scale=2, a=0.5, b=3.0)
  dtgamma(seq(0, 4, by=0.5), 1, scale=2, a=0.5, b=3.0)
  ptgamma(seq(0, 4, by=0.5), 1, scale=2, a=0.5, b=3.0)
  qtgamma(seq(0, 1, by=0.1), 1, scale=2, a=0.5, b=3.0)
}
\keyword{distribution}

