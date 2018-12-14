context("test-tweibull")

test_that("rtweibull", 
{
  n <- 100000
  x <- rtweibull(n, 3, scale = 1, a = 1, b = 10)
  expect_true(all(x >= 1))
  expect_true(all(x <= 10))
  expect_error(rtweibull(n, 3, -1), silent = TRUE)
})

test_that("dtweibull", 
{
  x <- seq(0, 3, by = 0.5)
  y <- dtweibull(x, 1, 2, 0.5, 2)
  expect_equal(y[c(1,6,7)], rep(0,3))
  expect_equal(y[2:5], dweibull(x[2:5], 1, scale = 2) /
                       (pweibull(2,1,scale = 2) - pweibull(0.5,1,scale = 2)))
  expect_error(dtweibull(x, 1, -1), silent = TRUE)
  expect_equal(dtweibull(7, shape = 1, scale = 1), dweibull(7, shape = 1, scale = 1))
})

test_that("ptweibull", 
{
  x <- seq(0, 3, by = 0.5)
  y <- ptweibull(x, 1, 2, 0.5, 2)
  expect_equal(y[1], 0)
  expect_equal(y[6:7], rep(1,2))
  z <- ptweibull(rtweibull(10, 1, 2, 0.5, 2), 1, 2, 0.5, 2)
  expect_true(all( z >= 0 & z <= 1))
  p <- seq(0,1,by = 0.1)
  expect_equal(ptweibull(qtweibull(p, 1, 2, 0.5, 2), 1, 2, 0.5, 2), p)
  expect_error(ptweibull(x, 1, -1), silent = TRUE)
  expect_equal(ptweibull(7, shape = 1, scale = 1), pweibull(7, shape = 1, scale = 1))
})

test_that("qtweibull", 
{
  p <- seq(0, 1, by = 0.1)
  x <- qtweibull(p, 1, 2, 0.5, 2)
  expect_equal(x[c(1,11)], c(0.5,2))
  x <- seq(0.5,2.0, by = 0.1)
  expect_equal(qtweibull(ptweibull(x, 1, 2, 0.5, 2), 1, 2, 0.5, 2), x)
  expect_error(qtweibull(x, 1, -1), silent = TRUE)
  expect_equal(qtweibull(0.5, shape = 1, scale = 1), qweibull(0.5, shape = 1, scale = 1))
})
