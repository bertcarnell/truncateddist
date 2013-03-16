\name{tlnorm}
\alias{tlnorm}
\alias{rtlnorm}
\alias{dtlnorm}
\alias{ptlnorm}
\alias{qtlnorm}
\title{Truncated Lognormal Functions}
\description{
  pdf, cdf, inverse cdf, and random deviates of the truncated lognormal distribution.
}
\usage{
rtlnorm(n, meanlog=0, sdlog=1, a=0, b=Inf)
dtlnorm(x, meanlog=0, sdlog=1, a=0, b=Inf)
ptlnorm(x, meanlog=0, sdlog=1, a=0, b=Inf)
qtlnorm(p, meanlog=0, sdlog=1, a=0, b=Inf)
}
\arguments{
  \item{x,q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If length(n) > 1, the length is taken to be the number required.}
  \item{meanlog}{vector of means on the natural log scale.}
  \item{sdlog}{vector of standard deviations on the natural log scale.}
  \item{a}{vector of lower truncation limits}
  \item{b}{vector of upper truncation limits}
}
\value{
  dtlnorm gives the density, ptlnorm gives the distribution function, qtlnorm gives the quantile function, and rtlnorm generates random deviates.
}
\author{Rob Carnell}
\examples{
  rtlnorm(5, 1, 2, 0.5, 3.0)
  dtlnorm(seq(0, 4, by=0.5), 1, 2, 0.5, 3.0)
  ptlnorm(seq(0, 4, by=0.5), 1, 2, 0.5, 3.0)
  qtlnorm(seq(0, 1, by=0.1), 1, 2, 0.5, 3.0)
}
\keyword{distribution}

