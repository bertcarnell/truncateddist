\name{tStrangeNorm}
\alias{tStrangeNorm}
\alias{rtStrangeNorm}
\alias{dtStrangeNorm}
\alias{ptStrangeNorm}
\alias{qtStrangeNorm}
\title{Truncated Delta Normal Functions}
\description{
  pdf, cdf, inverse cdf, and random deviates of the truncated Delta Normal distribution.
  The Delta Normal distribution has all the probability less than zero at zero.
}
\usage{
rtStrangeNorm(n, mean=0, sd=1, b=Inf)
dtStrangeNorm(x, mean=0, sd=1, b=Inf)
ptStrangeNorm(x, mean=0, sd=1, b=Inf)
qtStrangeNorm(p, mean=0, sd=1, b=Inf)
}
\arguments{
  \item{x,q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If length(n) > 1, the length is taken to be the number required.}
  \item{mean}{vector of means.}
  \item{sd}{vector of standard deviations.}
  \item{b}{vector of upper truncation limits}
}
\value{
  dtStrangeNorm gives the density, ptStrangeNorm gives the distribution function, qtStrangeNorm gives the quantile function, and rtStrangeNorm generates random deviates.
}
\author{Rob Carnell}
\examples{
  rtStrangeNorm(5, 1, 2, 3.0)
  dtStrangeNorm(seq(0, 4, by=0.5), 1, 2, 3.0)
  ptStrangeNorm(seq(0, 4, by=0.5), 1, 2, 3.0)
  qtStrangeNorm(seq(0, 1, by=0.1), 1, 2, 3.0)
}
\keyword{distribution}

