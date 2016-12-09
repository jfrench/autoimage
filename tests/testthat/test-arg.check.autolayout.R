test_that("sanity checks for arg.check.autolayout arguments", {
  # checking size has proper dimensions
  expect_error(arg.check.autolayout(1, "none", TRUE, TRUE, TRUE, 2, TRUE), 
               "size should be a vector of length 2")
  expect_error(arg.check.autolayout(1:3, "none", TRUE,TRUE, TRUE, 2, TRUE), 
               "size should be a vector of length 2")
  # checking that size has proper type
  expect_error(arg.check.autolayout(factor(1:2), TRUE, TRUE, TRUE, TRUE, 2, TRUE), 
               "size should be numeric")
  expect_error(arg.check.autolayout(letters[1:2], TRUE, TRUE, TRUE, TRUE, 2, TRUE), 
               "size should be numeric")
  # checking that size has values >= 1
  expect_error(arg.check.autolayout(c(.99, 1), TRUE, TRUE, TRUE, TRUE, 2, TRUE),
               "the elements of size should be positive integers")
  # check legend
  expect_error(arg.check.autolayout(1:2, c(TRUE, TRUE), TRUE, TRUE, TRUE, 2, TRUE),
               'invalid legend argument.  legend should be "none", "horizontal", or "vertical".')
  expect_error(arg.check.autolayout(1:2, try(match.call("jim", c("none", "horizontal", "vertical")), silent = TRUE), TRUE, TRUE, TRUE, 2, TRUE),
               'invalid legend argument.  legend should be "none", "horizontal", or "vertical".')
  # check common.legend
  expect_error(arg.check.autolayout(1:2, "none", c(TRUE, TRUE), TRUE, TRUE, 2, TRUE),
               "common.legend should be a single logical value")
  expect_error(arg.check.autolayout(1:2, "none", c(1, 2), TRUE, TRUE, 2, TRUE),
               "common.legend should be a single logical value")
  # check outer
  expect_error(arg.check.autolayout(1:2, "none", TRUE, c(TRUE, TRUE), TRUE, 2, TRUE),
               "outer should be a single logical value")
  expect_error(arg.check.autolayout(1:2, "none", TRUE, c(1, 2), TRUE, 2, TRUE),
               "outer should be a single logical value")
  # check show
  expect_error(arg.check.autolayout(1:2, "none", TRUE, TRUE, c(TRUE, TRUE), 2, TRUE),
               "show should be a single logical value")
  expect_error(arg.check.autolayout(1:2, "none", TRUE, TRUE, c(1, 2), 2, TRUE),
               "show should be a single logical value")
  # check mratio
  expect_error(arg.check.autolayout(1:2, "none", TRUE, TRUE, TRUE, 1:2, TRUE),
               "mratio should be a single positive number")
  expect_error(arg.check.autolayout(1:2, "none", TRUE, TRUE, TRUE, "test", TRUE),
               "mratio should be a single positive number")
  expect_error(arg.check.autolayout(1:2, "none", TRUE, TRUE, TRUE, -1, TRUE),
               "mratio should be a single positive number")
})
