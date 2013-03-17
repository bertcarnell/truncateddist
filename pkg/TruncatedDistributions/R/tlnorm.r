rtlnorm <- function(n, meanlog=0, sdlog=1, a=0, b=Inf)
{
  stopifnot(n > 0 & all(sdlog > 0))
  x <- runif(n)
  Fa <- plnorm(a, meanlog, sdlog)
  Fb <- plnorm(b, meanlog, sdlog)
  y <- (1-x)*Fa + x*Fb
  return(qlnorm(y, meanlog, sdlog))
}

dtlnorm <- function(x, meanlog=0, sdlog=1, a=0, b=Inf)
{
  stopifnot( all(sdlog > 0) )
  Fa <- plnorm(a, meanlog, sdlog)
  Fb <- plnorm(b, meanlog, sdlog)
  y <- dlnorm(x, meanlog, sdlog)
  inda <- which(x < a)
  indb <- which(x > b)
  if(length(inda) > 0) y[inda] <- 0
  if(length(indb) > 0) y[indb] <- 0
  return(y/(Fb-Fa))
}

ptlnorm <- function(x, meanlog=0, sdlog=1, a=0, b=Inf)
{
  stopifnot( all( sdlog > 0 ) )
  Fa <- plnorm(a, meanlog, sdlog)
  Fb <- plnorm(b, meanlog, sdlog)
  p <- ( plnorm(x, meanlog, sdlog) - Fa ) / ( Fb - Fa )
  inda <- which(x < a)
  indb <- which(x > b)
  if(length(inda) > 0) p[inda] <- 0
  if(length(indb) > 0) p[indb] <- 1
  return(p)
}

qtlnorm <- function(p, meanlog=0, sdlog=1, a=0, b=Inf)
{
  stopifnot( all(p >= 0 & p <= 1) &  all( sdlog > 0 ) )
  Fa <- plnorm(a, meanlog, sdlog)
  Fb <- plnorm(b, meanlog, sdlog)
  pNew <- p * (Fb - Fa) + Fa
  x <- qlnorm(pNew, meanlog, sdlog)
  return(x)
}
