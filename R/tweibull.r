#' Truncated weibull distribution functions
#' 
#' @description pdf, cdf, inverse cdf, and random deviates of the truncated weibull distribution.
#'
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param shape vector of shape parameters
#' @param scale vector of scale parameters.
#' @param a vector of lower truncation limits
#' @param b vector of upper truncation limits
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#'
#' @return dtweibull gives the density, ptweibull gives the distribution function, qtweibull gives the quantile function, and rtweibull generates random deviates.
#' @export
#' @rdname tweibull
#' @importFrom stats pweibull rweibull qweibull dweibull runif
#'
#' @examples
#' rtweibull(5, 1, scale=2, a=0.5, b=3.0)
#' dtweibull(seq(0, 4, by=0.5), 1, scale=2, a=0.5, b=3.0)
#' ptweibull(seq(0, 4, by=0.5), 1, scale=2, a=0.5, b=3.0)
#' qtweibull(seq(0, 1, by=0.1), 1, scale=2, a=0.5, b=3.0)
rtweibull <- function(n, shape, scale, a=0, b=Inf)
{
  stopifnot(n > 0 & all(scale > 0) & all(shape > 0))
  x <- runif(n)
  Fa <- pweibull(a, shape, scale)
  Fb <- pweibull(b, shape, scale)
  y <- (1 - x)*Fa + x*Fb
  return(qweibull(y, shape, scale))
}

#' @export
#' @rdname tweibull
dtweibull <- function(x, shape, scale, a=0, b=Inf)
{
  stopifnot( all(scale > 0) & all(shape > 0) )
  Fa <- pweibull(a, shape, scale)
  Fb <- pweibull(b, shape, scale)
  y <- dweibull(x, shape, scale)
  inda <- which(x < a)
  indb <- which(x > b)
  if (length(inda) > 0) y[inda] <- 0
  if (length(indb) > 0) y[indb] <- 0
  return(y/(Fb - Fa))
}

#' @export
#' @rdname tweibull
ptweibull <- function(q, shape, scale, a=0, b=Inf)
{
  stopifnot( all( scale > 0 ) & all(shape > 0) )
  Fa <- pweibull(a, shape, scale)
  Fb <- pweibull(b, shape, scale)
  p <- (pweibull(q, shape, scale) - Fa) / (Fb - Fa)
  inda <- which(q < a)
  indb <- which(q > b)
  if (length(inda) > 0) p[inda] <- 0
  if (length(indb) > 0) p[indb] <- 1
  return(p)
}

#' @export
#' @rdname tweibull
qtweibull <- function(p, shape, scale, a=0, b=Inf)
{
  stopifnot( all(p >= 0 & p <= 1) & all( scale > 0 ) & all(shape > 0) )
  Fa <- pweibull(a, shape, scale)
  Fb <- pweibull(b, shape, scale)
  pNew <- p * (Fb - Fa) + Fa
  x <- qweibull(pNew, shape, scale)
  return(x)
}
