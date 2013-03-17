rtnorm <- function(n, mean=0, sd=1, a=-Inf, b=Inf)
{
  stopifnot(n > 0 & all(sd > 0))
  x <- runif(n)
  Fa <- pnorm(a, mean, sd)
  Fb <- pnorm(b, mean, sd)
  y <- (1-x)*Fa + x*Fb
  return(qnorm(y, mean, sd))
}

dtnorm <- function(x, mean=0, sd=1, a=-Inf, b=Inf)
{
  stopifnot( all(sd > 0) )
  Fa <- pnorm(a, mean, sd)
  Fb <- pnorm(b, mean, sd)
  y <- dnorm(x, mean, sd)
  inda <- which(x < a)
  indb <- which(x > b)
  if(length(inda) > 0) y[inda] <- 0
  if(length(indb) > 0) y[indb] <- 0
  return(y/(Fb-Fa))
}

ptnorm <- function(x, mean=0, sd=1, a=-Inf, b=Inf)
{
  stopifnot( all( sd > 0 ) )
  Fa <- pnorm(a, mean, sd)
  Fb <- pnorm(b, mean, sd)
  p <- ( pnorm(x, mean, sd) - Fa ) / ( Fb - Fa )
  inda <- which(x < a)
  indb <- which(x > b)
  if(length(inda) > 0) p[inda] <- 0
  if(length(indb) > 0) p[indb] <- 1
  return(p)
}

qtnorm <- function(p, mean=0, sd=1, a=-Inf, b=Inf)
{
  stopifnot( all(p >= 0 & p <= 1) &  all( sd > 0 ) )
  Fa <- pnorm(a, mean, sd)
  Fb <- pnorm(b, mean, sd)
  pNew <- p * (Fb - Fa) + Fa
  x <- qnorm(pNew, mean, sd)
  return(x)
}
