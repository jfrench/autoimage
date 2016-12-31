data(narccap)
z <- tasmax[, , 1]
arglist <- list()
nr <- nrow(z)
nc <- ncol(z)

test_that("sanity checks for pimage.xyz.setup", {
  # need something matrix-like
  expect_error(pimage.xyz.setup(NULL, NULL, NULL, "", "", arglist), "no \"z\" matrix specified")
  expect_error(pimage.xyz.setup(1:10, NULL, NULL, "", "", arglist), "argument must be matrix-like")
  expect_error(pimage.xyz.setup(NULL, NULL, 1:10, "", "", arglist), "x and y must be specified when z is not a matrix")
  # x, y, z all vector no problem
  expect_error(pimage.xyz.setup(NULL, NULL, 1:10, "", "", arglist), "x and y must be specified when z is not a matrix")
  expect_silent(pimage.xyz.setup(rnorm(10), rnorm(10), rnorm(10), "", 
    "", arglist))  # okay
  # x and y length problems
  expect_error(pimage.xyz.setup(rnorm(9), rnorm(10), rnorm(10), "", "", 
    arglist), "x and y do not have the same length and/or dimensions")
  expect_error(pimage.xyz.setup(rnorm(9), rnorm(10), rnorm(10), "", "", 
    arglist), "x and y do not have the same length and/or dimensions")
  # matrix but not other arguments.  okay
  expect_silent(pimage.xyz.setup(NULL, NULL, tasmax[, , 1], "", "", arglist))
  expect_silent(pimage.xyz.setup(tasmax[, , 1], NULL, NULL, "", "", arglist))
  # regular grid okay
  expect_silent(pimage.xyz.setup(x = 1:nr, y = 1:nc, z = tasmax[, , 1], 
    "", "", arglist))
  # wrong x and y lengths
  expect_error(pimage.xyz.setup(x = 1:5, y = 1:nc, z = tasmax[, , 1], 
    "", "", arglist))
  expect_error(pimage.xyz.setup(x = 1:nr, y = 1:5, z = tasmax[, , 1], 
    "", "", arglist))
  # x and y matrix.  okay
  expect_silent(pimage.xyz.setup(x = matrix(rnorm(prod(dim(z))), nrow = nr), 
    y = matrix(rnorm(prod(dim(z))), nrow = nr), z = z, "", "", arglist))
  # x and y matrices bad
  expect_error(pimage.xyz.setup(x = matrix(1, nrow = (nr + 1), ncol = nc), 
    y = matrix(rnorm(prod(dim(z))), nrow = nr), z = z, "", "", arglist))
  expect_error(pimage.xyz.setup(x = matrix(1, nrow = nr, ncol = nc), 
    y = matrix(1, nrow = nr, ncol = (nc + 1)), z = z, "", "", arglist))
  expect_error(pimage.xyz.setup(x = matrix(1, nrow = nr, ncol = nc), 
    y = 1:nc, z = z, "", "", arglist))
})
TRUE
