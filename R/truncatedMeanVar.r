#' Mean and Variance of a truncated distribution
#'
#' @param f string name of a distribution (gamma, norm, beta, lnorm, weibull)
#' @param param1 the first distribution parameter
#' @param param2 the second distribution parameter
#' @param a lower truncation limit
#' @param b upper truncation limit
#'
#' @return a list containing the mean or variance and absolute error
#' @export
#' @rdname trunc-mean-var
#' @importFrom stats integrate
#'
#' @examples
#' meanTruncated("gamma", 1, 2, 0.1, 5)
#' varTruncated("gamma", 1, 2, 0.1, 5)
meanTruncated <- function(f, param1, param2, a, b)
{
  stopifnot(is.character(f))
  stopifnot(f %in% c("gamma","norm","beta","lnorm","weibull"))

  param2text <- ifelse(f == "gamma", "scale=param2", "param2")

  g <- function(x) eval(parse(text = paste("x * d", f, "(x, param1, ", param2text, ")", sep = "")))

  Fa <- eval(parse(text = paste("p", f, "(a, param1, ", param2text, ")", sep = "")))
  Fb <- eval(parse(text = paste("p", f, "(b, param1, ", param2text, ")", sep = "")))

  out <- integrate(g, a, b)
  
  list(value = out$value / (Fb - Fa), abs.error = out$abs.error / (Fb - Fa))
}

#' @export
#' @rdname trunc-mean-var
varTruncated <- function(f, param1, param2, a, b)
{
  stopifnot(is.character(f))
  stopifnot(f %in% c("gamma","norm","beta","lnorm","weibull"))

  param2text <- ifelse(f == "gamma", "scale=param2", "param2")

  g <- function(x) eval(parse(text = paste("x * x * d", f, "(x, param1, ", param2text, ")", sep = "")))

  Fa <- eval(parse(text = paste("p", f, "(a, param1, ", param2text, ")", sep = "")))
  Fb <- eval(parse(text = paste("p", f, "(b, param1, ", param2text, ")", sep = "")))

  out <- integrate(g, a, b)
  
  m <- meanTruncated(f, param1, param2, a, b)

  list(value = out$value / (Fb - Fa) - (m$value)^2,
       abs.error = out$abs.error / (Fb - Fa) + 2*m$value*m$abs.error)
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


