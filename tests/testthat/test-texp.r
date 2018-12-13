context("test-texp")

test_that("rtexp", 
{
  n <- 100000
  x <- rtexp(n, 3, a = 1, b = 10)
  expect_true(all(x >= 1))
  expect_true(all(x <= 10))
  expect_error(rtexp(n, -1), silent = TRUE)
})

test_that("dtexp", 
{
  x <- seq(0, 3, by = 0.5)
  y <- dtexp(x, 1, 0.5, 2)
  expect_equal(y[c(1,6,7)], rep(0,3))
  expect_equal(y[2:5], dexp(x[2:5], 1) /
                       (pexp(2,1) - pexp(0.5,1)))
  expect_error(dtexp(x, -1), silent = TRUE)
  expect_equal(dtexp(7, 1), dexp(7, 1))
})

test_that("ptexp", 
{
  x <- seq(0, 3, by = 0.5)
  y <- ptexp(x, 1, 0.5, 2)
  expect_equal(y[1], 0)
  expect_equal(y[6:7], rep(1,2))
  z <- ptexp(rtexp(10, 1, 0.5, 2), 1, 0.5, 2)
  expect_true(all( z >= 0 & z <= 1))
  p <- seq(0,1,by = 0.1)
  expect_equal(ptexp(qtexp(p, 2, 0.5, 2), 2, 0.5, 2), p)
  expect_error(ptexp(x, -1), silent = TRUE)
  expect_equal(ptexp(7, 1), pexp(7, 1))
})

test_that("qtexp", 
{
  p <- seq(0, 1, by = 0.1)
  x <- qtexp(p, 2, 0.5, 2)
  expect_equal(x[c(1,11)], c(0.5,2))
  x <- seq(0.5,2.0, by = 0.1)
  expect_equal(qtexp(ptexp(x, 2, 0.5, 2), 2, 0.5, 2), x)
  expect_error(qtexp(x, -1), silent = TRUE)
  expect_equal(qtexp(0.5, 1), qexp(0.5, 1))
})
