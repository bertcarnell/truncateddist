#' Truncated gamma distribution functions
#' 
#' @description pdf, cdf, inverse cdf, and random deviates of the truncated gamma distribution.
#'
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param shape vector of shape parameters.
#' @param scale vector of scale parameters.
#' @param a vector of lower truncation limits
#' @param b vector of upper truncation limits
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#'
#' @return dtgamma gives the density, ptgamma gives the distribution function, qtgamma gives the quantile function, and rtgamma generates random deviates.
#' @export
#' @rdname tgamma
#' @importFrom stats pgamma qgamma dgamma rgamma runif
#'
#' @examples
#' rtgamma(5, 1, scale=2, a=0.5, b=3.0)
#' dtgamma(seq(0, 4, by=0.5), 1, scale=2, a=0.5, b=3.0)
#' ptgamma(seq(0, 4, by=0.5), 1, scale=2, a=0.5, b=3.0)
#' qtgamma(seq(0, 1, by=0.1), 1, scale=2, a=0.5, b=3.0)
rtgamma <- function(n, shape, scale=1, a=0, b=Inf)
{
  stopifnot(n > 0 & all(shape > 0) & all(scale > 0))
  x <- runif(n)
  Fa <- pgamma(a, shape, scale=scale)
  Fb <- pgamma(b, shape, scale=scale)
  y <- (1-x)*Fa + x*Fb
  return(qgamma(y, shape, scale=scale))
}

#' @rdname tgamma
#' @export
dtgamma <- function(x, shape, scale=1, a=0, b=Inf)
{
  stopifnot(all(shape > 0) & all(scale > 0))
  Fa <- pgamma(a, shape, scale=scale)
  Fb <- pgamma(b, shape, scale=scale)
  y <- dgamma(x, shape, scale=scale)
  inda <- which(x < a)
  indb <- which(x > b)
  if(length(inda) > 0) y[inda] <- 0
  if(length(indb) > 0) y[indb] <- 0
  return(y/(Fb-Fa))
}

#' @rdname tgamma
#' @export
ptgamma <- function(q, shape, scale=1, a=0, b=Inf)
{
  stopifnot(all(shape > 0) & all(scale > 0))
  Fa <- pgamma(a, shape, scale=scale)
  Fb <- pgamma(b, shape, scale=scale)
  p <- ( pgamma(q, shape, scale=scale) - Fa ) / ( Fb - Fa )
  inda <- which(q < a)
  indb <- which(q > b)
  if(length(inda) > 0) p[inda] <- 0
  if(length(indb) > 0) p[indb] <- 1
  return(p)
}

#' @rdname tgamma
#' @export
qtgamma <- function(p, shape, scale=1, a=0, b=Inf)
{
  stopifnot( all(p >= 0 & p <= 1) & all(scale > 0 ) & all(shape > 0) )
  Fa <- pgamma(a, shape, scale=scale)
  Fb <- pgamma(b, shape, scale=scale)
  pNew <- p * (Fb - Fa) + Fa
  x <- qgamma(pNew, shape, scale=scale)
  return(x)
}
