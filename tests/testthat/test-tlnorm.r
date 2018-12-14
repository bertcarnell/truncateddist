context("test-tlnorm")

test_that("rtlnorm", 
{
  n <- 100000
  x <- rtlnorm(n, 0, 1, 1, 10)
  expect_true(all(x >= 1))
  expect_true(all(x <= 10))
  expect_error(rtlnorm(n, 0, -1), silent = TRUE)
})

test_that("dtlnorm", 
{
  x <- seq(0, 3, by = 0.5)
  y <- dtlnorm(x, 1, 2, 0.5, 2)
  expect_equal(y[c(1,6,7)], rep(0,3))
  expect_equal(y[2:5], dlnorm(x[2:5], 1, 2) /
                       (plnorm(2,1,2) - plnorm(0.5,1,2)))
  expect_error(dtlnorm(x, 1, -1), silent = TRUE)
  expect_equal(dtlnorm(7), dlnorm(7))
})

test_that("ptlnorm", 
{
  x <- seq(0, 3, by = 0.5)
  y <- ptlnorm(x, 1, 2, 0.5, 2)
  expect_equal(y[1], 0)
  expect_equal(y[6:7], rep(1,2))
  z <- ptlnorm(rtlnorm(10, 1, 2, 0.5, 2), 1, 2, 0.5, 2)
  expect_true(all( z >= 0 & z <= 1))
  p <- seq(0,1,by = 0.1)
  expect_equal(ptlnorm(qtlnorm(p, 1, 2, 0.5, 2), 1, 2, 0.5, 2), p)
  expect_error(ptlnorm(x, 1, -1), silent = TRUE)
  expect_equal(ptlnorm(7), plnorm(7))
})

test_that("qtlnorm", 
{
  p <- seq(0, 1, by = 0.1)
  x <- qtlnorm(p, 1, 2, 0.5, 2)
  expect_equal(x[c(1,11)], c(0.5,2))
  x <- seq(0.5,2.0, by = 0.1)
  expect_equal(qtlnorm(ptlnorm(x, 1, 2, 0.5, 2), 1, 2, 0.5, 2), x)
  expect_error(qtlnorm(x, 1, -1), silent = TRUE)
  expect_equal(qtlnorm(0.5), qlnorm(0.5))
})
