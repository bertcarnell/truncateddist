\name{tbeta}
\alias{tbeta}
\alias{rtbeta}
\alias{dtbeta}
\alias{ptbeta}
\alias{qtbeta}
\title{Truncated Beta Functions}
\description{
  pdf, cdf, inverse cdf, and random deviates of the truncated beta distribution.
}
\usage{
rtbeta(n, alpha, beta, a=0, b=1)
dtbeta(x, alpha, beta, a=0, b=1)
ptbeta(q, alpha, beta, a=0, b=1)
qtbeta(p, alpha, beta, a=0, b=1)
}
\arguments{
  \item{x,q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If length(n) > 1, the length is taken to be the number required.}
  \item{alpha}{vector of alpha parameters.}
  \item{beta}{vector of beta parameters.}
  \item{a}{vector of lower truncation limits}
  \item{b}{vector of upper truncation limits}
}
\value{
  dtbeta gives the density, ptbeta gives the distribution function, qtbeta gives the quantile function, and rtbeta generates random deviates.
}
\author{Rob Carnell}
\examples{
  rtbeta(5, 1, 2, a=0.5, b=0.9)
  dtbeta(seq(0, 1, by=0.1), 1, 2, a=0.5, b=0.9)
  ptbeta(seq(0, 1, by=0.1), 1, 2, a=0.5, b=0.9)
  qtbeta(seq(0, 1, by=0.1), 1, 2, a=0.5, b=0.9)
}
\keyword{distribution}

