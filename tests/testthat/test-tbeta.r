context("test-tbeta")

test_that("rtbeta", {
  n <- 100000
  x <- rtbeta(n, 2, 2, .1, .9)
  expect_true(all(x >= .1))
  expect_true(all(x <= .9))
  expect_error(rtweibull(n, -1, 2), silent = TRUE)
  expect_error(rtweibull(n, 2, -2), silent = TRUE)
  x <- rtbeta(n, 2, 2)
  expect_true(all(x >= 0))
  expect_true(all(x <= 1))
})

test_that("dtbeta", 
{
  x <- seq(0, 1, by = .01)
  y <- dtbeta(x, 2, 2, 0.1, 0.9)
  expect_equal(y[1:10], rep(0,10))
  expect_equal(y[92:101], rep(0,10))
  expect_equal(y[11:91], dbeta(x[11:91], 2, 2) /
                       (pbeta(.9,2,2) - pbeta(0.1,2,2)))
  expect_error(dtbeta(x, 1, -1), silent = TRUE)
  expect_error(dtbeta(x, -2, 2), silent = TRUE)
  expect_equal(dtbeta(.7, 2, 2), dbeta(.7, 2, 2))
})

test_that("ptbeta", 
{
  x <- seq(0, 1, by = 0.01)
  y <- ptbeta(x, 2, 2, 0.1, .9)
  expect_equal(y[1:10], rep(0,10))
  expect_equal(y[92:101], rep(1,10))
  z <- ptbeta(rtbeta(100, 1, 2, 0.5, .9), 1, 2, 0.5, .9)
  expect_true(all( z >= 0 & z <= 1))
  p <- seq(0,1,by = 0.1)
  expect_equal(ptbeta(qtbeta(p, 1, 2, 0.5, .9), 1, 2, 0.5, .9), p)
  expect_error(ptbeta(x, 1, -1), silent = TRUE)
  expect_error(ptbeta(x, -1, 1), silent = TRUE)
  expect_equal(ptbeta(.5, 2, 2), pbeta(.5, 2, 2))
})

test_that("qtbeta", 
{
  p <- seq(0, 1, by = 0.1)
  x <- qtbeta(p, 2, 2, 0.1, .9)
  expect_equal(x[c(1,11)], c(0.1, 0.9))
  x <- seq(0.1,0.9, by = 0.1)
  expect_equal(qtbeta(ptbeta(x, 2, 2, 0.1, 0.9), 2, 2, 0.1, 0.9), x)
  expect_error(qtbeta(x, 1, -1), silent = TRUE)
  expect_error(qtbeta(x, -1, 1), silent = TRUE)
  expect_equal(qtbeta(0.5, 2, 2), qbeta(0.5, 2, 2))
})

