\name{texp}
\alias{texp}
\alias{rtexp}
\alias{dtexp}
\alias{ptexp}
\alias{qtexp}
\title{Truncated Exponential Functions}
\description{
  pdf, cdf, inverse cdf, and random deviates of the truncated Exponential distribution.
}
\usage{
rtexp(n, rate, a=-Inf, b=Inf)
dtexp(x, rate, a=-Inf, b=Inf)
ptexp(x, rate, a=-Inf, b=Inf)
qtexp(p, rate, a=-Inf, b=Inf)
}
\arguments{
  \item{x,q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If length(n) > 1, the length is taken to be the number required.}
  \item{rate}{vector of rate parameters.}
  \item{a}{vector of lower truncation limits}
  \item{b}{vector of upper truncation limits}
}
\value{
  dtexp gives the density, ptexp gives the distribution function, qtexp gives the quantile function, and rtexp generates random deviates.
}
\author{Rob Carnell}
\examples{
  rtexp(5, 1, a=0.5, b=3.0)
  dtexp(seq(0, 4, by=0.5), 1, a=0.5, b=3.0)
  ptexp(seq(0, 4, by=0.5), 1, a=0.5, b=3.0)
  qtexp(seq(0, 1, by=0.1), 1, a=0.5, b=3.0)
}
\keyword{distribution}

