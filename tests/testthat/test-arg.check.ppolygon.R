context("arg.check.ppolygon tests")

test_that("sanity checks for arg.check.ppolygon arguments", {
  # checking size has proper dimensions
  expect_error(arg.check.ppolygon(list(x = 1), NULL, proj = "none"), 
    "If x is a list, it should have arguments x and y")
  expect_error(arg.check.ppolygon(list(x = 1, y = 1:2), NULL, proj = "none"))
  expect_error(arg.check.ppolygon(list(x = 1, y = "jim"), NULL, proj = "none"), 
    "x and y should be numeric")
  expect_error(arg.check.ppolygon(x = 1, y = "jim", proj = "none"), "x and y should be numeric")
  expect_error(arg.check.ppolygon(x = 1, y = 1:2, proj = "none"), "x and y should have the same length")
  expect_error(arg.check.ppolygon(x = 1:2, y = 1:2, proj = c("a", "b")), 
    "proj should be a single character string")
  expect_error(arg.check.ppolygon(x = 1:2, y = 1:2, proj = 8), "proj should be a single character string")
})
TRUE
