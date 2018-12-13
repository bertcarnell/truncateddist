context("test-tgamma")

test_that("rtgamma", 
{
  n <- 100000
  x <- rtgamma(n, 3, scale = 1, a = 1, b = 10)
  expect_true(all(x >= 1))
  expect_true(all(x <= 10))
  expect_error(rtgamma(n, 3, -1), silent = TRUE)
})

test_that("dtgamma", 
{
  x <- seq(0, 3, by = 0.5)
  y <- dtgamma(x, 1, 2, 0.5, 2)
  expect_equal(y[c(1,6,7)], rep(0,3))
  expect_equal(y[2:5], dgamma(x[2:5], 1, scale = 2) /
                       (pgamma(2,1,scale = 2) - pgamma(0.5,1,scale = 2)))
  expect_error(dtgamma(x, 1, -1), silent = TRUE)
  expect_equal(dtgamma(7, shape = 1), dgamma(7, shape = 1))
})

test_that("ptgamma", 
{
  x <- seq(0, 3, by = 0.5)
  y <- ptgamma(x, 1, 2, 0.5, 2)
  expect_equal(y[1], 0)
  expect_equal(y[6:7], rep(1,2))
  z <- ptgamma(rtgamma(10, 1, 2, 0.5, 2), 1, 2, 0.5, 2)
  expect_true(all( z >= 0 & z <= 1))
  p <- seq(0,1,by = 0.1)
  expect_equal(ptgamma(qtgamma(p, 1, 2, 0.5, 2), 1, 2, 0.5, 2), p)
  expect_error(ptgamma(x, 1, -1), silent = TRUE)
  expect_equal(ptgamma(7, shape = 1), pgamma(7, shape = 1))
})

test_that("qtgamma", 
{
  p <- seq(0, 1, by = 0.1)
  x <- qtgamma(p, 1, 2, 0.5, 2)
  expect_equal(x[c(1,11)], c(0.5,2))
  x <- seq(0.5,2.0, by = 0.1)
  expect_equal(qtgamma(ptgamma(x, 1, 2, 0.5, 2), 1, 2, 0.5, 2), x)
  expect_error(qtgamma(x, 1, -1), silent = TRUE)
  expect_equal(qtgamma(0.5, shape = 1), qgamma(0.5, shape = 1))
})
