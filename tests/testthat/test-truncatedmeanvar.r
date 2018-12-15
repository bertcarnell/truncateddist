context("test-truncatedmeanvar")

test_that("multiplication works", {
  expect_equal(1*2, meanTruncated("gamma", 1, 2, 0.0, Inf)$value)
  expect_true(meanTruncated("gamma", 1, 2, 0.1, 5)$value < 1*2)
  expect_equal(1*2^2, varTruncated("gamma", 1, 2, 0.0, Inf)$value)
  expect_true(varTruncated("gamma", 1, 2, 0.1, 5)$value < 1*2^2)
})
