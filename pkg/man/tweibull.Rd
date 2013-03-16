\name{tweibull}
\alias{tweibull}
\alias{rtweibull}
\alias{dtweibull}
\alias{ptweibull}
\alias{qtweibull}
\title{Truncated weibull Functions}
\description{
  pdf, cdf, inverse cdf, and random deviates of the truncated weibull distribution.
}
\usage{
rtweibull(n, shape, scale, a=-Inf, b=Inf)
dtweibull(x, shape, scale, a=-Inf, b=Inf)
ptweibull(x, shape, scale, a=-Inf, b=Inf)
qtweibull(p, shape, scale, a=-Inf, b=Inf)
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
  dtweibull gives the density, ptweibull gives the distribution function, qtweibull gives the quantile function, and rtweibull generates random deviates.
}
\author{Rob Carnell}
\examples{
  rtweibull(5, 1, scale=2, a=0.5, b=3.0)
  dtweibull(seq(0, 4, by=0.5), 1, scale=2, a=0.5, b=3.0)
  ptweibull(seq(0, 4, by=0.5), 1, scale=2, a=0.5, b=3.0)
  qtweibull(seq(0, 1, by=0.1), 1, scale=2, a=0.5, b=3.0)
}
\keyword{distribution}

