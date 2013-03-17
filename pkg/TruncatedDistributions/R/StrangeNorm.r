rStrangeNorm <- function(n, mean=0, sd=1)
{
  stopifnot(sd > 0)
  x <- rnorm(n, mean, sd)
  ind <- which(x < 0)
  x[ind] <- 0
  return(x)
}

dStrangeNorm <- function(x, mean=0, sd=1)
{
  stopifnot(sd > 0)
  ind <- which(x < 0)
  ind2 <- which(x == 0)
  y <- dnorm(x, mean, sd)
  if(length(ind) > 0) y[ind] <- 0
  if(length(ind2) > 0) y[ind2] <- pnorm(0, mean, sd)
  return(y)
}

pStrangeNorm <- function(x, mean=0, sd=1)
{
  stopifnot(sd > 0)
  ind <- which(x < 0)
  p <- pnorm(x, mean, sd)
  if(length(ind) > 0) p[ind] <- 0
  return(p)
}

qStrangeNorm <- function(p, mean=0, sd=1)
{
  stopifnot(sd > 0 & p <= 1 & p >= 0)
  ind <- which(p < pnorm(0, mean, sd))
  x <- qnorm(p, mean, sd)
  if(length(ind) > 0) x[ind] <- 0
  return(x)
}
