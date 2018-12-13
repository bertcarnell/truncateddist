#' Truncated beta distribution functions
#' 
#' @description pdf, cdf, inverse cdf, and random deviates of the truncated beta distribution.
#'
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param alpha vector of alpha parameters.
#' @param beta vector of beta parameters.
#' @param a vector of lower truncation limits
#' @param b vector of upper truncation limits
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#'
#' @return dtbeta gives the density, ptbeta gives the distribution function, qtbeta gives the quantile function, and rtbeta generates random deviates.
#' @export
#' @rdname tbeta
#' @importFrom stats pbeta rbeta dbeta qbeta runif
#'
#' @examples
#' set.seed(1976)
#' rtbeta(10, 3, 4, 0.2, 0.8)
#' dtbeta(0.5, 3, 4, 0.2, 0.7)
#' qtbeta(0.1, 3, 4, 0.2, 0.9)
#' ptbeta(0.3, 3, 4, 0.2, 0.9)
#' rtbeta(5, 1, 2, a=0.5, b=0.9)
#' dtbeta(seq(0, 1, by=0.1), 1, 2, a=0.5, b=0.9)
#' ptbeta(seq(0, 1, by=0.1), 1, 2, a=0.5, b=0.9)
#' qtbeta(seq(0, 1, by=0.1), 1, 2, a=0.5, b=0.9)
rtbeta <- function(n, alpha, beta, a=0, b=1)
{
  stopifnot(n > 0 & all(beta > 0) & all(alpha > 0))
  x <- runif(n)
  Fa <- pbeta(a, alpha, beta)
  Fb <- pbeta(b, alpha, beta)
  y <- (1 - x)*Fa + x*Fb
  return(qbeta(y, alpha, beta))
}

#' @rdname tbeta
#' @export
dtbeta <- function(x, alpha, beta, a=0, b=1)
{
  stopifnot( all(alpha > 0) & all(beta > 0) )
  Fa <- pbeta(a, alpha, beta)
  Fb <- pbeta(b, alpha, beta)
  y <- dbeta(x, alpha, beta)
  inda <- which(x < a)
  indb <- which(x > b)
  if (length(inda) > 0) y[inda] <- 0
  if (length(indb) > 0) y[indb] <- 0
  return(y/(Fb - Fa))
}

#' @rdname tbeta
#' @export
ptbeta <- function(q, alpha, beta, a=0, b=1)
{
  stopifnot( all( alpha > 0 ) & all(beta > 0) )
  Fa <- pbeta(a, alpha, beta)
  Fb <- pbeta(b, alpha, beta)
  p <- (pbeta(q, alpha, beta) - Fa) / (Fb - Fa)
  inda <- which(q < a)
  indb <- which(q > b)
  if (length(inda) > 0) p[inda] <- 0
  if (length(indb) > 0) p[indb] <- 1
  return(p)
}

#' @rdname tbeta
#' @export
qtbeta <- function(p, alpha, beta, a=0, b=1)
{
  stopifnot( all(p >= 0 & p <= 1) &  all( alpha > 0 ) & all(beta > 0) )
  Fa <- pbeta(a, alpha, beta)
  Fb <- pbeta(b, alpha, beta)
  pNew <- p * (Fb - Fa) + Fa
  x <- qbeta(pNew, alpha, beta)
  return(x)
}
