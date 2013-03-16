################################################################################
# Program Name:  runit_testRiskTreeEnineInput.R
# Purpose:       Test necessary functions to create tree files
# Charge:        CG920579-ST1
# Author:        Rob Carnell
# Date:          December 2006
#
# R version:     >= 2.3.1
#
################################################################################

#/**
# /testedfunction x
# /testedpurpose  x
# /test           1. x
# /test           2. x
#*/
test.rtnorm <- function()
{
  n <- 100000
  x <- rtnorm(n, 0, 1, -0.5, 0.5)
  checkTrue(all(x >= -0.5))
  checkTrue(all(x <= 0.5))
  checkException(rtnorm(n, 0, -1), silent=TRUE)
}

test.dtnorm <- function()
{
  x <- seq(0, 3, by=0.5)
  y <- dtnorm(x, 1, 2, 0.5, 2)
  checkEqualsNumeric(y[c(1,6,7)], rep(0,3))
  checkEqualsNumeric(y[2:5], dnorm(x[2:5], 1, 2) /
                     (pnorm(2,1,2) - pnorm(0.5,1,2)))
  checkException(dtnorm(x, 1, -1), silent=TRUE)
  checkEqualsNumeric(dtnorm(7), dnorm(7))
}

test.ptnorm <- function()
{
  x <- seq(0, 3, by=0.5)
  y <- ptnorm(x, 1, 2, 0.5, 2)
  checkEqualsNumeric(y[1], 0)
  checkEqualsNumeric(y[6:7], rep(1,2))
  z <- ptnorm(rtnorm(10, 1, 2, 0.5, 2), 1, 2, 0.5, 2)
  checkTrue(all( z >= 0 & z <= 1))
  p <- seq(0,1,by=0.1)
  checkEqualsNumeric(ptnorm(qtnorm(p, 1, 2, 0.5, 2), 1, 2, 0.5, 2), p)
  checkException(ptnorm(x, 1, -1), silent=TRUE)
  checkEqualsNumeric(ptnorm(7), pnorm(7))
}

test.qtnorm <- function()
{
  p <- seq(0, 1, by=0.1)
  x <- qtnorm(p, 1, 2, 0.5, 2)
  checkEqualsNumeric(x[c(1,11)], c(0.5,2))
  x <- seq(0.5,2.0, by=0.1)
  checkEqualsNumeric(qtnorm(ptnorm(x, 1, 2, 0.5, 2), 1, 2, 0.5, 2), x)
  checkException(qtnorm(x, 1, -1), silent=TRUE)
  checkEqualsNumeric(qtnorm(0.5), qnorm(0.5))
}

################################################################################

test.rtlnorm <- function()
{
  n <- 100000
  x <- rtlnorm(n, 0, 1, 1, 10)
  checkTrue(all(x >= 1))
  checkTrue(all(x <= 10))
  checkException(rtlnorm(n, 0, -1), silent=TRUE)
}

test.dtlnorm <- function()
{
  x <- seq(0, 3, by=0.5)
  y <- dtlnorm(x, 1, 2, 0.5, 2)
  checkEqualsNumeric(y[c(1,6,7)], rep(0,3))
  checkEqualsNumeric(y[2:5], dlnorm(x[2:5], 1, 2) /
                     (plnorm(2,1,2) - plnorm(0.5,1,2)))
  checkException(dtlnorm(x, 1, -1), silent=TRUE)
  checkEqualsNumeric(dtlnorm(7), dlnorm(7))
}

test.ptlnorm <- function()
{
  x <- seq(0, 3, by=0.5)
  y <- ptlnorm(x, 1, 2, 0.5, 2)
  checkEqualsNumeric(y[1], 0)
  checkEqualsNumeric(y[6:7], rep(1,2))
  z <- ptlnorm(rtlnorm(10, 1, 2, 0.5, 2), 1, 2, 0.5, 2)
  checkTrue(all( z >= 0 & z <= 1))
  p <- seq(0,1,by=0.1)
  checkEqualsNumeric(ptlnorm(qtlnorm(p, 1, 2, 0.5, 2), 1, 2, 0.5, 2), p)
  checkException(ptlnorm(x, 1, -1), silent=TRUE)
  checkEqualsNumeric(ptlnorm(7), plnorm(7))
}

test.qtlnorm <- function()
{
  p <- seq(0, 1, by=0.1)
  x <- qtlnorm(p, 1, 2, 0.5, 2)
  checkEqualsNumeric(x[c(1,11)], c(0.5,2))
  x <- seq(0.5,2.0, by=0.1)
  checkEqualsNumeric(qtlnorm(ptlnorm(x, 1, 2, 0.5, 2), 1, 2, 0.5, 2), x)
  checkException(qtlnorm(x, 1, -1), silent=TRUE)
  checkEqualsNumeric(qtlnorm(0.5), qlnorm(0.5))
}

################################################################################

test.rtgamma <- function()
{
  n <- 100000
  x <- rtgamma(n, 3, scale=1, a=1, b=10)
  checkTrue(all(x >= 1))
  checkTrue(all(x <= 10))
  checkException(rtgamma(n, 3, -1), silent=TRUE)
}

test.dtgamma <- function()
{
  x <- seq(0, 3, by=0.5)
  y <- dtgamma(x, 1, 2, 0.5, 2)
  checkEqualsNumeric(y[c(1,6,7)], rep(0,3))
  checkEqualsNumeric(y[2:5], dgamma(x[2:5], 1, scale=2) /
                     (pgamma(2,1,scale=2) - pgamma(0.5,1,scale=2)))
  checkException(dtgamma(x, 1, -1), silent=TRUE)
  checkEqualsNumeric(dtgamma(7, shape=1), dgamma(7, shape=1))
}

test.ptgamma <- function()
{
  x <- seq(0, 3, by=0.5)
  y <- ptgamma(x, 1, 2, 0.5, 2)
  checkEqualsNumeric(y[1], 0)
  checkEqualsNumeric(y[6:7], rep(1,2))
  z <- ptgamma(rtgamma(10, 1, 2, 0.5, 2), 1, 2, 0.5, 2)
  checkTrue(all( z >= 0 & z <= 1))
  p <- seq(0,1,by=0.1)
  checkEqualsNumeric(ptgamma(qtgamma(p, 1, 2, 0.5, 2), 1, 2, 0.5, 2), p)
  checkException(ptgamma(x, 1, -1), silent=TRUE)
  checkEqualsNumeric(ptgamma(7, shape=1), pgamma(7, shape=1))
}

test.qtgamma <- function()
{
  p <- seq(0, 1, by=0.1)
  x <- qtgamma(p, 1, 2, 0.5, 2)
  checkEqualsNumeric(x[c(1,11)], c(0.5,2))
  x <- seq(0.5,2.0, by=0.1)
  checkEqualsNumeric(qtgamma(ptgamma(x, 1, 2, 0.5, 2), 1, 2, 0.5, 2), x)
  checkException(qtgamma(x, 1, -1), silent=TRUE)
  checkEqualsNumeric(qtgamma(0.5, shape=1), qgamma(0.5, shape=1))
}


################################################################################

test.rtweibull <- function()
{
  n <- 100000
  x <- rtweibull(n, 3, scale=1, a=1, b=10)
  checkTrue(all(x >= 1))
  checkTrue(all(x <= 10))
  checkException(rtweibull(n, 3, -1), silent=TRUE)
}

test.dtweibull <- function()
{
  x <- seq(0, 3, by=0.5)
  y <- dtweibull(x, 1, 2, 0.5, 2)
  checkEqualsNumeric(y[c(1,6,7)], rep(0,3))
  checkEqualsNumeric(y[2:5], dweibull(x[2:5], 1, scale=2) /
                     (pweibull(2,1,scale=2) - pweibull(0.5,1,scale=2)))
  checkException(dtweibull(x, 1, -1), silent=TRUE)
  checkEqualsNumeric(dtweibull(7, shape=1, scale=1), dweibull(7, shape=1, scale=1))
}

test.ptweibull <- function()
{
  x <- seq(0, 3, by=0.5)
  y <- ptweibull(x, 1, 2, 0.5, 2)
  checkEqualsNumeric(y[1], 0)
  checkEqualsNumeric(y[6:7], rep(1,2))
  z <- ptweibull(rtweibull(10, 1, 2, 0.5, 2), 1, 2, 0.5, 2)
  checkTrue(all( z >= 0 & z <= 1))
  p <- seq(0,1,by=0.1)
  checkEqualsNumeric(ptweibull(qtweibull(p, 1, 2, 0.5, 2), 1, 2, 0.5, 2), p)
  checkException(ptweibull(x, 1, -1), silent=TRUE)
  checkEqualsNumeric(ptweibull(7, shape=1, scale=1), pweibull(7, shape=1, scale=1))
}

test.qtweibull <- function()
{
  p <- seq(0, 1, by=0.1)
  x <- qtweibull(p, 1, 2, 0.5, 2)
  checkEqualsNumeric(x[c(1,11)], c(0.5,2))
  x <- seq(0.5,2.0, by=0.1)
  checkEqualsNumeric(qtweibull(ptweibull(x, 1, 2, 0.5, 2), 1, 2, 0.5, 2), x)
  checkException(qtweibull(x, 1, -1), silent=TRUE)
  checkEqualsNumeric(qtweibull(0.5, shape=1, scale=1), qweibull(0.5, shape=1, scale=1))
}

################################################################################

test.rStrangeNorm <- function()
{
  n <- 100000
  x <- rStrangeNorm(n, 0, 1)
  checkTrue(all(x >= 0))
  checkException(rStrangeNorm(n, 0, -1), silent=TRUE)
}

test.dStrangeNorm <- function()
{
  x <- seq(0, 3, by=0.5)
  y <- dStrangeNorm(x, 0, 2)
  checkEqualsNumeric(y[1], 0.5)
  checkEqualsNumeric(y[2:7], dnorm(x[2:7], 0, 2))
  checkException(dStrangeNorm(x, 1, -1), silent=TRUE)
  checkEqualsNumeric(dStrangeNorm(-1), 0)
  checkEqualsNumeric(dStrangeNorm(3), dnorm(3))
}

test.pStrangeNorm <- function()
{
  x <- seq(0, 3, by=0.5)
  y <- pStrangeNorm(x, 0, 2)
  checkEqualsNumeric(y[1], 0.5)
  checkEqualsNumeric(y[2:7], pnorm(x[2:7], 0, 2))
  z <- pStrangeNorm(rStrangeNorm(10, 1, 2), 1, 2)
  checkTrue(all( z >= 0 & z <= 1))
  p <- seq(0,1,by=0.1)
  checkEqualsNumeric(pStrangeNorm(qStrangeNorm(p, 1, 2), 1, 2),
                     c(rep(pnorm(0,1,2), 4), p[5:11]))
  checkException(pStrangeNorm(x, 1, -1), silent=TRUE)
  checkEqualsNumeric(pStrangeNorm(7), pnorm(7))
  checkEqualsNumeric(pStrangeNorm(-1), 0)
}

test.qStrangeNorm <- function()
{
  p <- seq(0, 1, by=0.1)
  x <- qStrangeNorm(p, 1, 2)
  checkEqualsNumeric(x[1:4], rep(0,4))
  checkEqualsNumeric(x[5:11], qnorm(p[5:11], 1, 2))
  x <- seq(0.5,2.0, by=0.1)
  checkEqualsNumeric(qStrangeNorm(pStrangeNorm(x, 1, 2), 1, 2), x)
  checkException(qStrangeNorm(x, 1, -1), silent=TRUE)
  checkEqualsNumeric(qStrangeNorm(0.5), qnorm(0.5))
  checkEqualsNumeric(qStrangeNorm(0), 0)
}

################################################################################

test.rtbeta <- function()
{
  n <- 100000
  x <- rtbeta(n, 2, 2, .1, .9)
  checkTrue(all(x >= .1))
  checkTrue(all(x <= .9))
  checkException(rtweibull(n, -1, 2), silent=TRUE)
  checkException(rtweibull(n, 2, -2), silent=TRUE)
  x <- rtbeta(n, 2, 2)
  checkTrue(all(x >= 0))
  checkTrue(all(x <= 1))
}

test.dtbeta <- function()
{
  x <- seq(0, 1, by=.01)
  y <- dtbeta(x, 2, 2, 0.1, 0.9)
  checkEqualsNumeric(y[1:10], rep(0,10))
  checkEqualsNumeric(y[92:101], rep(0,10))
  checkEqualsNumeric(y[11:91], dbeta(x[11:91], 2, 2) /
                     (pbeta(.9,2,2) - pbeta(0.1,2,2)))
  checkException(dtbeta(x, 1, -1), silent=TRUE)
  checkException(dtbeta(x, -2, 2), silent=TRUE)
  checkEqualsNumeric(dtbeta(.7, 2, 2), dbeta(.7, 2, 2))
}

test.ptbeta <- function()
{
  x <- seq(0, 1, by=0.01)
  y <- ptbeta(x, 2, 2, 0.1, .9)
  checkEqualsNumeric(y[1:10], rep(0,10))
  checkEqualsNumeric(y[92:101], rep(1,10))
  z <- ptbeta(rtbeta(100, 1, 2, 0.5, .9), 1, 2, 0.5, .9)
  checkTrue(all( z >= 0 & z <= 1))
  p <- seq(0,1,by=0.1)
  checkEqualsNumeric(ptbeta(qtbeta(p, 1, 2, 0.5, .9), 1, 2, 0.5, .9), p)
  checkException(ptbeta(x, 1, -1), silent=TRUE)
  checkException(ptbeta(x, -1, 1), silent=TRUE)
  checkEqualsNumeric(ptbeta(.5, 2, 2), pbeta(.5, 2, 2))
}

test.qtbeta <- function()
{
  p <- seq(0, 1, by=0.1)
  x <- qtbeta(p, 2, 2, 0.1, .9)
  checkEqualsNumeric(x[c(1,11)], c(0.1, 0.9))
  x <- seq(0.1,0.9, by=0.1)
  checkEqualsNumeric(qtbeta(ptbeta(x, 2, 2, 0.1, 0.9), 2, 2, 0.1, 0.9), x)
  checkException(qtbeta(x, 1, -1), silent=TRUE)
  checkException(qtbeta(x, -1, 1), silent=TRUE)
  checkEqualsNumeric(qtbeta(0.5, 2, 2), qbeta(0.5, 2, 2))
}

################################################################################

test.rtexp <- function()
{
  n <- 100000
  x <- rtexp(n, 3, a=1, b=10)
  checkTrue(all(x >= 1))
  checkTrue(all(x <= 10))
  checkException(rtexp(n, -1), silent=TRUE)
}

test.dtexp <- function()
{
  x <- seq(0, 3, by=0.5)
  y <- dtexp(x, 1, 0.5, 2)
  checkEqualsNumeric(y[c(1,6,7)], rep(0,3))
  checkEqualsNumeric(y[2:5], dexp(x[2:5], 1) /
                     (pexp(2,1) - pexp(0.5,1)))
  checkException(dtexp(x, -1), silent=TRUE)
  checkEqualsNumeric(dtexp(7, 1), dexp(7, 1))
}

test.ptexp <- function()
{
  x <- seq(0, 3, by=0.5)
  y <- ptexp(x, 1, 0.5, 2)
  checkEqualsNumeric(y[1], 0)
  checkEqualsNumeric(y[6:7], rep(1,2))
  z <- ptexp(rtexp(10, 1, 0.5, 2), 1, 0.5, 2)
  checkTrue(all( z >= 0 & z <= 1))
  p <- seq(0,1,by=0.1)
  checkEqualsNumeric(ptexp(qtexp(p, 2, 0.5, 2), 2, 0.5, 2), p)
  checkException(ptexp(x, -1), silent=TRUE)
  checkEqualsNumeric(ptexp(7, 1), pexp(7, 1))
}

test.qtexp <- function()
{
  p <- seq(0, 1, by=0.1)
  x <- qtexp(p, 2, 0.5, 2)
  checkEqualsNumeric(x[c(1,11)], c(0.5,2))
  x <- seq(0.5,2.0, by=0.1)
  checkEqualsNumeric(qtexp(ptexp(x, 2, 0.5, 2), 2, 0.5, 2), x)
  checkException(qtexp(x, -1), silent=TRUE)
  checkEqualsNumeric(qtexp(0.5, 1), qexp(0.5, 1))
}
