#' Truncated exponential distribution functions
#' 
#' @description pdf, cdf, inverse cdf, and random deviates of the truncated Exponential distribution.
#'
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param rate vector of rate parameters.
#' @param a vector of lower truncation limits
#' @param b vector of upper truncation limits
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#'
#' @return dtexp gives the density, ptexp gives the distribution function, qtexp gives the quantile function, and rtexp generates random deviates.
#' @export
#' @rdname texp
#' @importFrom stats rexp dexp qexp pexp runif
#'
#' @examples
#' rtexp(5, 1, a=0.5, b=3.0)
#' dtexp(seq(0, 4, by=0.5), 1, a=0.5, b=3.0)
#' ptexp(seq(0, 4, by=0.5), 1, a=0.5, b=3.0)
#' qtexp(seq(0, 1, by=0.1), 1, a=0.5, b=3.0)
rtexp <- function(n, rate, a=0, b=Inf)
{
  stopifnot(n > 0 & all(rate > 0))
  x <- runif(n)
  Fa <- pexp(a, rate)
  Fb <- pexp(b, rate)
  y <- (1 - x)*Fa + x*Fb
  return(qexp(y, rate))
}

#' @rdname texp
#' @export
dtexp <- function(x, rate, a=0, b=Inf)
{
  stopifnot(all(rate > 0))
  Fa <- pexp(a, rate)
  Fb <- pexp(b, rate)
  y <- dexp(x, rate)
  inda <- which(x < a)
  indb <- which(x > b)
  if (length(inda) > 0) y[inda] <- 0
  if (length(indb) > 0) y[indb] <- 0
  return(y/(Fb - Fa))
}

#' @rdname texp
#' @export
ptexp <- function(q, rate, a=0, b=Inf)
{
  stopifnot( all( rate > 0 ) )
  Fa <- pexp(a, rate)
  Fb <- pexp(b, rate)
  p <- (pexp(q, rate) - Fa) / (Fb - Fa)
  inda <- which(q < a)
  indb <- which(q > b)
  if (length(inda) > 0) p[inda] <- 0
  if (length(indb) > 0) p[indb] <- 1
  return(p)
}

#' @rdname texp
#' @export
qtexp <- function(p, rate, a=0, b=Inf)
{
  stopifnot( all(p >= 0 & p <= 1) & all( rate > 0 ) )
  Fa <- pexp(a, rate)
  Fb <- pexp(b, rate)
  pNew <- p * (Fb - Fa) + Fa
  x <- qexp(pNew, rate)
  return(x)
}
