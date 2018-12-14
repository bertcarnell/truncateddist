context("test-tnorm")

test_that("rtnorm", 
{
  n <- 100000
  x <- rtnorm(n, 0, 1, -0.5, 0.5)
  expect_true(all(x >= -0.5))
  expect_true(all(x <= 0.5))
  expect_error(rtnorm(n, 0, -1), silent = TRUE)
})

test_that("dtnorm", 
{
  x <- seq(0, 3, by = 0.5)
  y <- dtnorm(x, 1, 2, 0.5, 2)
  expect_equal(y[c(1,6,7)], rep(0,3))
  expect_equal(y[2:5], dnorm(x[2:5], 1, 2) /
                       (pnorm(2,1,2) - pnorm(0.5,1,2)))
  expect_error(dtnorm(x, 1, -1), silent = TRUE)
  expect_equal(dtnorm(7), dnorm(7))
})

test_that("ptnorm", 
{
  x <- seq(0, 3, by = 0.5)
  y <- ptnorm(x, 1, 2, 0.5, 2)
  expect_equal(y[1], 0)
  expect_equal(y[6:7], rep(1,2))
  z <- ptnorm(rtnorm(10, 1, 2, 0.5, 2), 1, 2, 0.5, 2)
  expect_true(all( z >= 0 & z <= 1))
  p <- seq(0,1,by = 0.1)
  expect_equal(ptnorm(qtnorm(p, 1, 2, 0.5, 2), 1, 2, 0.5, 2), p)
  expect_error(ptnorm(x, 1, -1), silent = TRUE)
  expect_equal(ptnorm(7), pnorm(7))
})

test_that("qtnorm", 
{
  p <- seq(0, 1, by = 0.1)
  x <- qtnorm(p, 1, 2, 0.5, 2)
  expect_equal(x[c(1,11)], c(0.5,2))
  x <- seq(0.5,2.0, by = 0.1)
  expect_equal(qtnorm(ptnorm(x, 1, 2, 0.5, 2), 1, 2, 0.5, 2), x)
  expect_error(qtnorm(x, 1, -1), silent = TRUE)
  expect_equal(qtnorm(0.5), qnorm(0.5))
})
