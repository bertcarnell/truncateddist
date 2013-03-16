\name{StrangeNorm}
\alias{StrangeNorm}
\alias{rStrangeNorm}
\alias{dStrangeNorm}
\alias{pStrangeNorm}
\alias{qStrangeNorm}
\title{Delta Normal Functions}
\description{
  pdf, cdf, inverse cdf, and random deviates of the Delta Normal distribution.
  The Delta Normal distribution has all the probability less than zero at zero.
}
\usage{
rStrangeNorm(n, mean=0, sd=1)
dStrangeNorm(x, mean=0, sd=1)
pStrangeNorm(x, mean=0, sd=1)
qStrangeNorm(p, mean=0, sd=1)
}
\arguments{
  \item{x,q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If length(n) > 1, the length is taken to be the number required.}
  \item{mean}{vector of means.}
  \item{sd}{vector of standard deviations.}
}
\value{
  dStrangeNorm gives the density, pStrangeNorm gives the distribution function, qStrangeNorm gives the quantile function, and rStrangeNorm generates random deviates.
}
\author{Rob Carnell}
\examples{
  rStrangeNorm(5, 1, 2)
  dStrangeNorm(seq(0, 4, by=0.5), 1, 2)
  pStrangeNorm(seq(0, 4, by=0.5), 1, 2)
  qStrangeNorm(seq(0, 1, by=0.1), 1, 2)
}
\keyword{distribution}

