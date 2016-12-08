test_that("sanity checks for arg.check.autolayout arguments", {
  # checking size has proper dimensions
  expect_error(arg.check.autolayout(1, TRUE, TRUE, TRUE, TRUE, TRUE, 2), 
               "size should be a vector of length 2")
  expect_error(arg.check.autolayout(1:3, TRUE, TRUE, TRUE,TRUE, TRUE, 2), 
               "size should be a vector of length 2")
  # checking that size has proper type
  expect_error(arg.check.autolayout(factor(1:2), TRUE, TRUE, TRUE, TRUE, TRUE, 2), 
               "size should be numeric")
  expect_error(arg.check.autolayout(letters[1:2], TRUE, TRUE, TRUE, TRUE, TRUE, 2), 
               "size should be numeric")
  # checking that size has values >= 1
  expect_error(arg.check.autolayout(c(.99, 1), TRUE, TRUE, TRUE, TRUE, TRUE, 2),
               "the elements of size should be positive integers")
  # check legend
  expect_error(arg.check.autolayout(1:2, c(TRUE, TRUE), TRUE, TRUE, TRUE, TRUE, 2),
               "legend should be a single logical value")
  expect_error(arg.check.autolayout(1:2, c(1, 2), TRUE, TRUE, TRUE, TRUE, 2),
               "legend should be a single logical value")
  # check horizontal
  expect_error(arg.check.autolayout(1:2, FALSE, c(TRUE, TRUE), TRUE, TRUE, TRUE, 2),
               "horizontal should be a single logical value")
  expect_error(arg.check.autolayout(1:2, TRUE, c(1, 2), TRUE, TRUE, TRUE, 2),
               "horizontal should be a single logical value")
  # check common.legend
  expect_error(arg.check.autolayout(1:2, FALSE, TRUE, c(TRUE, TRUE), TRUE, TRUE, 2),
               "common.legend should be a single logical value")
  expect_error(arg.check.autolayout(1:2, TRUE, TRUE, c(1, 2), TRUE, TRUE, 2),
               "common.legend should be a single logical value")
  # check outer
  expect_error(arg.check.autolayout(1:2, FALSE, TRUE, TRUE, c(TRUE, TRUE), TRUE, 2),
               "outer should be a single logical value")
  expect_error(arg.check.autolayout(1:2, TRUE, TRUE, TRUE, c(1, 2), TRUE, 2),
               "outer should be a single logical value")
  # check show
  expect_error(arg.check.autolayout(1:2, FALSE, FALSE, TRUE, TRUE, c(TRUE, TRUE), 2),
               "show should be a single logical value")
  expect_error(arg.check.autolayout(1:2, TRUE, TRUE, TRUE, TRUE, c(1, 2), 2),
               "show should be a single logical value")
  # check mratio
  expect_error(arg.check.autolayout(1:2, FALSE, FALSE, TRUE, TRUE, TRUE, 1:2),
               "mratio should be a single positive number")
  expect_error(arg.check.autolayout(1:2, FALSE, FALSE, TRUE, TRUE, TRUE, "test"),
               "mratio should be a single positive number")
  expect_error(arg.check.autolayout(1:2, FALSE, FALSE, TRUE, TRUE, TRUE, -1),
               "mratio should be a single positive number")
})
