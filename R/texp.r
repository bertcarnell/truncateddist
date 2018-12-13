rtexp <- function(n, rate, a=0, b=Inf)
{
  stopifnot(n > 0 & all(rate > 0))
  x <- runif(n)
  Fa <- pexp(a, rate)
  Fb <- pexp(b, rate)
  y <- (1-x)*Fa + x*Fb
  return(qexp(y, rate))
}

dtexp <- function(x, rate, a=0, b=Inf)
{
  stopifnot(all(rate > 0))
  Fa <- pexp(a, rate)
  Fb <- pexp(b, rate)
  y <- dexp(x, rate)
  inda <- which(x < a)
  indb <- which(x > b)
  if(length(inda) > 0) y[inda] <- 0
  if(length(indb) > 0) y[indb] <- 0
  return(y/(Fb-Fa))
}

ptexp <- function(q, rate, a=0, b=Inf)
{
  stopifnot( all( rate > 0 ) )
  Fa <- pexp(a, rate)
  Fb <- pexp(b, rate)
  p <- ( pexp(q, rate) - Fa ) / ( Fb - Fa )
  inda <- which(q < a)
  indb <- which(q > b)
  if(length(inda) > 0) p[inda] <- 0
  if(length(indb) > 0) p[indb] <- 1
  return(p)
}

qtexp <- function(p, rate, a=0, b=Inf)
{
  stopifnot( all(p >= 0 & p <= 1) & all( rate > 0 ) )
  Fa <- pexp(a, rate)
  Fb <- pexp(b, rate)
  pNew <- p * (Fb - Fa) + Fa
  x <- qexp(pNew, rate)
  return(x)
}
