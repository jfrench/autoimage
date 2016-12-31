test_that("sanity checks for arg.check.ptext arguments", {
  # checking size has proper dimensions
  expect_error(arg.check.ptext(list(x = 1), NULL, labels = letters[1], 
    proj = "none"), "If x is a list, it should have arguments x and y")
  expect_error(arg.check.ptext(list(x = 1, y = 1:2), NULL, labels = letters[1], 
    proj = "none"))
  expect_error(arg.check.ptext(list(x = 1, y = "jim"), NULL, labels = letters[1], 
    proj = "none"), "x and y should be numeric")
  expect_error(arg.check.ptext(x = 1, y = "jim", labels = letters[1], 
    proj = "none"), "x and y should be numeric")
  expect_error(arg.check.ptext(x = 1, y = 1:2, labels = letters[1], proj = "none"), 
    "x and y should have the same length")
  expect_error(arg.check.ptext(x = 1:2, y = 1:2, labels = letters[1], 
    proj = "none"), "labels length does not match length of x")
  expect_error(arg.check.ptext(x = 1:2, y = 1:2, labels = 1:2, proj = "none"), 
    "labels must be a character vector")
  expect_error(arg.check.ptext(x = 1:2, y = 1:2, labels = letters[1:2], 
    proj = c("a", "b")), "proj should be a single character string")
  expect_error(arg.check.ptext(x = 1:2, y = 1:2, labels = letters[1:2], 
    proj = 8), "proj should be a single character string")
})
TRUE
