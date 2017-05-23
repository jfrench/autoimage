context("arg.check.plines tests")

test_that("sanity checks for arg.check.plines arguments", {
  # checking size has proper dimensions
  expect_error(arg.check.plines(list(x = 1), NULL, type = "l", proj = "none"), 
    "If x is a list, it should have arguments x and y")
  expect_error(arg.check.plines(list(x = 1, y = 1:2), NULL, type = "l", 
    proj = "none"))
  expect_error(arg.check.plines(list(x = 1, y = "jim"), NULL, type = "l", 
    proj = "none"), "x and y should be numeric")
  expect_error(arg.check.plines(x = 1, y = "jim", type = "l", proj = "none"), 
    "x and y should be numeric")
  expect_error(arg.check.plines(x = 1, y = 1:2, type = "l", proj = "none"), 
    "x and y should have the same length")
  expect_error(arg.check.plines(x = 1:2, y = 1:2, type = "q", proj = "none"), 
    "invalid type argument")
  expect_error(arg.check.plines(x = 1:2, y = 1:2, type = "l", proj = c("a", 
    "b")), "proj should be a single character string")
  expect_error(arg.check.plines(x = 1:2, y = 1:2, type = "l", proj = 8), 
    "proj should be a single character string")
})