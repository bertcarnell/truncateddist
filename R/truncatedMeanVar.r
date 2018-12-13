meanTruncated <- function(f, param1, param2, a, b)
{
  stopifnot(is.character(f))
  stopifnot(f %in% c("gamma","norm","beta","lnorm","weibull"))

  param2text <- ifelse(f=="gamma", "scale=param2", "param2")

  g <- function(x) eval(parse(text=paste("x * d", f, "(x, param1, ", param2text, ")", sep="")))

  Fa <- eval(parse(text=paste("p", f, "(a, param1, ", param2text, ")", sep="")))
  Fb <- eval(parse(text=paste("p", f, "(b, param1, ", param2text, ")", sep="")))

  out <- integrate(g, a, b)
  
  list(value=out$value / (Fb - Fa), abs.error=out$abs.error / (Fb - Fa))
}

varTruncated <- function(f, param1, param2, a, b)
{
  stopifnot(is.character(f))
  stopifnot(f %in% c("gamma","norm","beta","lnorm","weibull"))

  param2text <- ifelse(f=="gamma", "scale=param2", "param2")

  g <- function(x) eval(parse(text=paste("x * x * d", f, "(x, param1, ", param2text, ")", sep="")))

  Fa <- eval(parse(text=paste("p", f, "(a, param1, ", param2text, ")", sep="")))
  Fb <- eval(parse(text=paste("p", f, "(b, param1, ", param2text, ")", sep="")))

  out <- integrate(g, a, b)
  
  m <- meanTruncated(f, param1, param2, a, b)

  list(value=out$value / (Fb - Fa) - (m$value)^2,
       abs.error=out$abs.error / (Fb - Fa) + 2*m$value*m$abs.error)
}

#a <- 0.1
#b <- 5
#alpha_ <- 1
#beta_ <- 2

#meanTruncated("gamma", alpha_, beta_, a, b)
#varTruncated("gamma", alpha_, beta_, a, b)

#alpha_ * beta_ *
#  (pgamma(b / beta_, alpha_ + 1, 1) - pgamma(a / beta_, alpha_ + 1, 1)) /
#  (pgamma(b / beta_, alpha_, 1) - pgamma(a / beta_, alpha_, 1))

#meanTruncated("gamma", alpha_, beta_, 0, Inf)
#alpha_*beta_
#varTruncated("gamma", alpha_, beta_, 0, Inf)
#alpha_*beta_^2

#alpha_ * beta_ *
#  (pgamma(Inf / beta_, alpha_ + 1, 1) - pgamma(0 / beta_, alpha_ + 1, 1)) /
#  (pgamma(Inf / beta_, alpha_, 1) - pgamma(0 / beta_, alpha_, 1))


