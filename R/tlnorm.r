#' Truncated lognormal distribution functions
#' 
#' @description pdf, cdf, inverse cdf, and random deviates of the truncated lognormal distribution.
#'
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param meanlog vector of means on the natural log scale.
#' @param sdlog vector of standard deviations on the natural log scale.
#' @param a vector of lower truncation limits
#' @param b vector of upper truncation limits
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#'
#' @return dtlnorm gives the density, ptlnorm gives the distribution function, qtlnorm gives the quantile function, and rtlnorm generates random deviates.
#' @export
#' @rdname tlnorm
#' @importFrom stats rlnorm dlnorm qlnorm plnorm runif
#'
#' @examples
#'  rtlnorm(5, 1, 2, 0.5, 3.0)
#'  dtlnorm(seq(0, 4, by=0.5), 1, 2, 0.5, 3.0)
#'  ptlnorm(seq(0, 4, by=0.5), 1, 2, 0.5, 3.0)
#'  qtlnorm(seq(0, 1, by=0.1), 1, 2, 0.5, 3.0)
rtlnorm <- function(n, meanlog=0, sdlog=1, a=0, b=Inf)
{
  stopifnot(n > 0 & all(sdlog > 0))
  x <- runif(n)
  Fa <- plnorm(a, meanlog, sdlog)
  Fb <- plnorm(b, meanlog, sdlog)
  y <- (1 - x)*Fa + x*Fb
  return(qlnorm(y, meanlog, sdlog))
}

#' @rdname tlnorm
#' @export
dtlnorm <- function(x, meanlog=0, sdlog=1, a=0, b=Inf)
{
  stopifnot( all(sdlog > 0) )
  Fa <- plnorm(a, meanlog, sdlog)
  Fb <- plnorm(b, meanlog, sdlog)
  y <- dlnorm(x, meanlog, sdlog)
  inda <- which(x < a)
  indb <- which(x > b)
  if (length(inda) > 0) y[inda] <- 0
  if (length(indb) > 0) y[indb] <- 0
  return(y/(Fb - Fa))
}

#' @rdname tlnorm
#' @export
ptlnorm <- function(q, meanlog=0, sdlog=1, a=0, b=Inf)
{
  stopifnot( all( sdlog > 0 ) )
  Fa <- plnorm(a, meanlog, sdlog)
  Fb <- plnorm(b, meanlog, sdlog)
  p <- (plnorm(q, meanlog, sdlog) - Fa) / (Fb - Fa)
  inda <- which(q < a)
  indb <- which(q > b)
  if (length(inda) > 0) p[inda] <- 0
  if (length(indb) > 0) p[indb] <- 1
  return(p)
}

#' @rdname tlnorm
#' @export
qtlnorm <- function(p, meanlog=0, sdlog=1, a=0, b=Inf)
{
  stopifnot( all(p >= 0 & p <= 1) &  all( sdlog > 0 ) )
  Fa <- plnorm(a, meanlog, sdlog)
  Fb <- plnorm(b, meanlog, sdlog)
  pNew <- p * (Fb - Fa) + Fa
  x <- qlnorm(pNew, meanlog, sdlog)
  return(x)
}
