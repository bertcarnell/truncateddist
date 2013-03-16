rtweibull <- function(n, shape, scale, a=0, b=Inf)
{
  stopifnot(n > 0 & all(scale > 0) & all(shape > 0))
  x <- runif(n)
  Fa <- pweibull(a, shape, scale)
  Fb <- pweibull(b, shape, scale)
  y <- (1-x)*Fa + x*Fb
  return(qweibull(y, shape, scale))
}

dtweibull <- function(x, shape, scale, a=0, b=Inf)
{
  stopifnot( all(scale > 0) & all(shape > 0) )
  Fa <- pweibull(a, shape, scale)
  Fb <- pweibull(b, shape, scale)
  y <- dweibull(x, shape, scale)
  inda <- which(x < a)
  indb <- which(x > b)
  if(length(inda) > 0) y[inda] <- 0
  if(length(indb) > 0) y[indb] <- 0
  return(y/(Fb-Fa))
}

ptweibull <- function(x, shape, scale, a=0, b=Inf)
{
  stopifnot( all( scale > 0 ) & all(shape > 0) )
  Fa <- pweibull(a, shape, scale)
  Fb <- pweibull(b, shape, scale)
  p <- ( pweibull(x, shape, scale) - Fa ) / ( Fb - Fa )
  inda <- which(x < a)
  indb <- which(x > b)
  if(length(inda) > 0) p[inda] <- 0
  if(length(indb) > 0) p[indb] <- 1
  return(p)
}

qtweibull <- function(p, shape, scale, a=0, b=Inf)
{
  stopifnot( all(p >= 0 & p <= 1) & all( scale > 0 ) & all(shape > 0) )
  Fa <- pweibull(a, shape, scale)
  Fb <- pweibull(b, shape, scale)
  pNew <- p * (Fb - Fa) + Fa
  x <- qweibull(pNew, shape, scale)
  return(x)
}
