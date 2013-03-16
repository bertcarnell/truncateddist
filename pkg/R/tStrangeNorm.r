rtStrangeNorm <- function(n, mean=0, sd=1, b=Inf)
{
  stopifnot(sd > 0)
  x <- runif(n)
  Fa <- 0
  Fb <- pnorm(b, mean, sd)
  y <- (1-x)*Fa + x*Fb
  z <- qnorm(y, mean, sd)
  ind <- which(z < 0)
  z[ind] <- 0
  return(z)
}

dtStrangeNorm <- function(x, mean=0, sd=1, b=Inf)
{
  stopifnot(sd > 0)
  ind <- which(x < 0)
  ind2 <- which(x == 0)
  Fa <- 0
  Fb <- pnorm(b, mean, sd)
  y <- dnorm(x, mean, sd)
  y <- y / (Fb-Fa)
  if(length(ind) > 0) y[ind] <- 0
  if(length(ind2) > 0) y[ind2] <- pnorm(0, mean, sd)
  return(y)
}

ptStrangeNorm <- function(x, mean=0, sd=1, b=Inf)
{
  stopifnot(sd > 0)
  Fb <- pnorm(b, mean, sd)
  p <- pnorm(x, mean, sd) / Fb
  ind <- which(x < 0)
  ind2 <- which(x >= b)
  if(length(ind) > 0) p[ind] <- 0
  if(length(ind2) > 0) p[ind2] <- 1
  return(p)
}

qtStrangeNorm <- function(p, mean=0, sd=1, b=Inf)
{
  stopifnot(sd > 0 & p <= 1 & p >= 0)
  Fb <- pnorm(b, mean, sd)
  ind <- which(p < pnorm(0, mean, sd) / Fb)
  x <- qnorm(p*Fb, mean, sd)
  if(length(ind) > 0) x[ind] <- 0
  return(x)
}
