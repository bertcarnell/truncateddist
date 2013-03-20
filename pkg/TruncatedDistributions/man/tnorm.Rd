\name{tnorm}
\alias{tnorm}
\alias{rtnorm}
\alias{dtnorm}
\alias{ptnorm}
\alias{qtnorm}
\title{Truncated Normal Functions}
\description{
  pdf, cdf, inverse cdf, and random deviates of the truncated normal distribution.
}
\usage{
rtnorm(n, mean=0, sd=1, a=-Inf, b=Inf)
dtnorm(x, mean=0, sd=1, a=-Inf, b=Inf)
ptnorm(q, mean=0, sd=1, a=-Inf, b=Inf)
qtnorm(p, mean=0, sd=1, a=-Inf, b=Inf)
}
\arguments{
  \item{x,q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If length(n) > 1, the length is taken to be the number required.}
  \item{mean}{vector of means.}
  \item{sd}{vector of standard deviations.}
  \item{a}{vector of lower truncation limits}
  \item{b}{vector of upper truncation limits}
}
\value{
  dtnorm gives the density, ptnorm gives the distribution function, qtnorm gives the quantile function, and rtnorm generates random deviates.
}
\author{Rob Carnell}
\examples{
  rtnorm(5, 1, 2, 0.5, 3.0)
  dtnorm(seq(0, 4, by=0.5), 1, 2, 0.5, 3.0)
  ptnorm(seq(0, 4, by=0.5), 1, 2, 0.5, 3.0)
  qtnorm(seq(0, 1, by=0.1), 1, 2, 0.5, 3.0)
}
\keyword{distribution}

