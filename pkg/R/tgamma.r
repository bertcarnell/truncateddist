rtgamma <- function(n, shape, scale=1, a=0, b=Inf)
{
  stopifnot(n > 0 & all(shape > 0) & all(scale > 0))
  x <- runif(n)
  Fa <- pgamma(a, shape, scale=scale)
  Fb <- pgamma(b, shape, scale=scale)
  y <- (1-x)*Fa + x*Fb
  return(qgamma(y, shape, scale=scale))
}

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

ptgamma <- function(x, shape, scale=1, a=0, b=Inf)
{
  stopifnot(all(shape > 0) & all(scale > 0))
  Fa <- pgamma(a, shape, scale=scale)
  Fb <- pgamma(b, shape, scale=scale)
  p <- ( pgamma(x, shape, scale=scale) - Fa ) / ( Fb - Fa )
  inda <- which(x < a)
  indb <- which(x > b)
  if(length(inda) > 0) p[inda] <- 0
  if(length(indb) > 0) p[indb] <- 1
  return(p)
}

qtgamma <- function(p, shape, scale=1, a=0, b=Inf)
{
  stopifnot( all(p >= 0 & p <= 1) & all(scale > 0 ) & all(shape > 0) )
  Fa <- pgamma(a, shape, scale=scale)
  Fb <- pgamma(b, shape, scale=scale)
  pNew <- p * (Fb - Fa) + Fa
  x <- qgamma(pNew, shape, scale=scale)
  return(x)
}
