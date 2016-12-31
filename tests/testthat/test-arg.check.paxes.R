context("arg.check.paxes tests")

test_that("sanity checks for arg.check.paxes arguments", {
  # check proj argument
  expect_error(arg.check.paxes(1:2, 1:2, 1:2, NULL, NULL, TRUE), "proj should be a single character string")
  expect_error(arg.check.paxes(1, 1:2, 1:2, NULL, NULL, TRUE), "proj should be a single character string")
  # check xlim argument
  expect_error(arg.check.paxes("none", 1:3, 1:2, NULL, NULL, TRUE), "xlim should be a numeric vector of length 2")
  expect_error(arg.check.paxes("none", letters[1:2], 1:2, NULL, NULL, 
    TRUE), "xlim should be a numeric vector of length 2")
  # check ylim argument
  expect_error(arg.check.paxes("none", 1:2, 1:3, NULL, NULL, TRUE), "ylim should be a numeric vector of length 2")
  expect_error(arg.check.paxes("none", 1:2, letters[1:2], NULL, NULL, 
    TRUE), "ylim should be a numeric vector of length 2")
  # check xaxp and yaxp argument
  expect_error(arg.check.paxes("none", 1:2, 1:2, 1:4, NULL, TRUE))
  expect_error(arg.check.paxes("none", 1:2, 1:2, 1:3, 1:4, TRUE))
  # check grid argument
  expect_error(arg.check.paxes("none", 1:2, 1:2, 1:3, 1:3, c(TRUE, TRUE)), 
    "grid should be a logical value")
  expect_error(arg.check.paxes("none", 1:2, 1:2, 1:3, 1:3, 7), "grid should be a logical value")
})

TRUE
