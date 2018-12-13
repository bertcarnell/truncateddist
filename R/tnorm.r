#' Truncated normal distribution functions
#' 
#' @description pdf, cdf, inverse cdf, and random deviates of the truncated normal distribution.
#'
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param mean vector of means.
#' @param sd vector of standard deviations.
#' @param a vector of lower truncation limits
#' @param b vector of upper truncation limits
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#'
#' @return dtnorm gives the density, ptnorm gives the distribution function, qtnorm gives the quantile function, and rtnorm generates random deviates.
#' @export
#' @rdname tnorm
#' @importFrom stats pnorm rnorm qnorm dnorm runif
#'
#' @examples
#' rtnorm(5, 1, 2, 0.5, 3.0)
#' dtnorm(seq(0, 4, by=0.5), 1, 2, 0.5, 3.0)
#' ptnorm(seq(0, 4, by=0.5), 1, 2, 0.5, 3.0)
#' qtnorm(seq(0, 1, by=0.1), 1, 2, 0.5, 3.0)
rtnorm <- function(n, mean=0, sd=1, a=-Inf, b=Inf)
{
  stopifnot(n > 0 & all(sd > 0))
  x <- runif(n)
  Fa <- pnorm(a, mean, sd)
  Fb <- pnorm(b, mean, sd)
  y <- (1 - x)*Fa + x*Fb
  return(qnorm(y, mean, sd))
}

#' @rdname tnorm
#' @export
dtnorm <- function(x, mean=0, sd=1, a=-Inf, b=Inf)
{
  stopifnot( all(sd > 0) )
  Fa <- pnorm(a, mean, sd)
  Fb <- pnorm(b, mean, sd)
  y <- dnorm(x, mean, sd)
  inda <- which(x < a)
  indb <- which(x > b)
  if (length(inda) > 0) y[inda] <- 0
  if (length(indb) > 0) y[indb] <- 0
  return(y/(Fb - Fa))
}

#' @rdname tnorm
#' @export
ptnorm <- function(q, mean=0, sd=1, a=-Inf, b=Inf)
{
  stopifnot( all( sd > 0 ) )
  Fa <- pnorm(a, mean, sd)
  Fb <- pnorm(b, mean, sd)
  p <- (pnorm(q, mean, sd) - Fa) / (Fb - Fa)
  inda <- which(q < a)
  indb <- which(q > b)
  if (length(inda) > 0) p[inda] <- 0
  if (length(indb) > 0) p[indb] <- 1
  return(p)
}

#' @rdname tnorm
#' @export
qtnorm <- function(p, mean=0, sd=1, a=-Inf, b=Inf)
{
  stopifnot( all(p >= 0 & p <= 1) &  all( sd > 0 ) )
  Fa <- pnorm(a, mean, sd)
  Fb <- pnorm(b, mean, sd)
  pNew <- p * (Fb - Fa) + Fa
  x <- qnorm(pNew, mean, sd)
  return(x)
}
