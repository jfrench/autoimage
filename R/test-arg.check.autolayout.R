test_that("sanity checks for arg.check.autolayout arguments", {
  # checking size has proper dimensions
  expect_error(arg.check.autolayout(1, TRUE, TRUE, TRUE, TRUE))
  expect_error(arg.check.autolayout(1:3, TRUE, TRUE, TRUE, TRUE))
  # checking that size has proper type
  expect_error(arg.check.autolayout(data.frame(c(1, 2)), TRUE, TRUE, TRUE, TRUE))
  expect_error(arg.check.autolayout(factor(1:2), TRUE, TRUE, TRUE, TRUE))
  # checking that size has values >= 1
  expect_error(arg.check.autolayout(c(.99, 1), TRUE, TRUE, TRUE, TRUE))
  # check legend
  expect_error(arg.check.autolayout(1:2, c(TRUE, TRUE), TRUE, TRUE, TRUE))
  expect_error(arg.check.autolayout(1:2, c(1, 2), TRUE, TRUE, TRUE))
  # check common.legend
  expect_error(arg.check.autolayout(1:2, FALSE, c(TRUE, TRUE), TRUE, TRUE))
  expect_error(arg.check.autolayout(1:2, TRUE, c(1, 2), TRUE, TRUE))
  # check common.legend
  expect_error(arg.check.autolayout(1:2, FALSE, TRUE, c(TRUE, TRUE), TRUE))
  expect_error(arg.check.autolayout(1:2, TRUE, TRUE, c(1, 2), TRUE))
  # check show
  expect_error(arg.check.autolayout(1:2, FALSE, FALSE, TRUE, c(TRUE, TRUE)))
  expect_error(arg.check.autolayout(1:2, TRUE, TRUE, TRUE, c(1, 2)))
})
