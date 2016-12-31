tx <- ""
ty <- ""
arglist <- list()
nr <- nrow(tasmax)
nc <- ncol(tasmax)
data(co, package = "gear")
test_that("autoimage.xyz.setup detects correct type of problem", {
  # single irregular grid
  expect_message(autoimage.xyz.setup(lon, lat, tasmax[, , 1], tx, ty, 
    arglist, TRUE), "note: a single irregular grid detected")
  # multiple irregular grid
  expect_message(autoimage.xyz.setup(lon, lat, tasmax[, , 1:2], tx, ty, 
    arglist, TRUE), "note: sequence of irregular grids detected")
  # single regular grid
  expect_message(autoimage.xyz.setup(1:nr, 1:nc, tasmax[, , 1], tx, ty, 
    arglist, TRUE), "note: a single regular grid detected")
  # multiple regular grid
  expect_message(autoimage.xyz.setup(1:nr, 1:nc, tasmax[, , 1:2], tx, 
    ty, arglist, TRUE), "note: sequence of regular grids detected")
  # single irregularly-spaced
  expect_message(autoimage.xyz.setup(co$long, co$lat, co$Al, tx, ty, 
    arglist, TRUE), "note: a single set of irregularly-spaced points detected")
  # multiple irregularly-spaced
  expect_message(autoimage.xyz.setup(co$long, co$lat, co[c("Al", "Ca")], 
    tx, ty, arglist, TRUE), "note: sequence of irregularly-spaced points detected")
})

test_that("sanity checks for autoimage.xyz.setup", {
  # need something matrix-like
  expect_error(autoimage.xyz.setup(NULL, NULL, NULL, "", "", arglist, 
    TRUE), "no \"z\" matrix specified")
  expect_error(autoimage.xyz.setup(1:10, NULL, NULL, "", "", arglist, 
    TRUE), "argument must be matrix-like")
  expect_error(autoimage.xyz.setup(NULL, NULL, 1:10, "", "", arglist, 
    TRUE), "x and y must be specified when z is not a matrix", TRUE)
  expect_error(autoimage.xyz.setup(NULL, NULL, 1:10, "", "", arglist, 
    TRUE), "x and y must be specified when z is not a matrix")
  expect_message(autoimage.xyz.setup(rnorm(10), rnorm(10), rnorm(10), 
    "", "", arglist, TRUE))  # okay
  # x and y length problems
  expect_error(autoimage.xyz.setup(rnorm(9), rnorm(10), rnorm(10), "", 
    "", arglist, TRUE), "x and y do not have the same length and/or dimensions")
  expect_error(autoimage.xyz.setup(rnorm(9), rnorm(10), rnorm(10), "", 
    "", arglist, TRUE), "x and y do not have the same length and/or dimensions")
  # matrix but not other arguments.  okay
  expect_message(autoimage.xyz.setup(NULL, NULL, tasmax[, , 1], "", "", 
    arglist, TRUE))
  expect_message(autoimage.xyz.setup(tasmax[, , 1], NULL, NULL, "", "", 
    arglist, TRUE))
  
  # regular grid okay
  expect_message(autoimage.xyz.setup(x = 1:nr, y = 1:nc, z = tasmax[, 
    , 1], "", "", arglist, TRUE))
  
  # wrong x and y lengths
  expect_error(autoimage.xyz.setup(x = 1:5, y = 1:nc, z = tasmax[, , 
    1], "", "", arglist, TRUE))
  expect_error(autoimage.xyz.setup(x = 1:nr, y = 1:5, z = tasmax[, , 
    1], "", "", arglist, TRUE))
  
  # x and y matrix.  okay
  z <- tasmax[, , 1:2]
  nc <- ncol(z)
  nr <- nrow(z)
  expect_message(autoimage.xyz.setup(x = matrix(rnorm(nr * nc), nrow = nr), 
    y = matrix(rnorm(nr * nc), nrow = nr), z = z, "", "", arglist, 
    TRUE))
  
  # x and y matrices bad
  expect_error(autoimage.xyz.setup(x = matrix(1, nrow = (nr + 1), ncol = nc), 
    y = matrix(rnorm(prod(dim(z))), nrow = nr), z = z, "", "", arglist, 
    TRUE))
  expect_error(autoimage.xyz.setup(x = matrix(1, nrow = nr, ncol = nc), 
    y = matrix(1, nrow = nr, ncol = (nc + 1)), z = z, "", "", arglist, 
    TRUE))
  expect_error(autoimage.xyz.setup(x = matrix(1, nrow = nr, ncol = nc), 
    y = 1:nc, z = z, "", "", arglist, TRUE))
  
  # test common.legend
  expect_error(autoimage.xyz.setup(x = matrix(1, nrow = (nr + 1), ncol = nc), 
    y = matrix(rnorm(prod(dim(z))), nrow = nr), z = z, "", "", arglist, 
    TRUE, c(TRUE, TRUE)), "common.legend should be a single value")
  expect_error(autoimage.xyz.setup(x = matrix(1, nrow = (nr + 1), ncol = nc), 
    y = matrix(rnorm(prod(dim(z))), nrow = nr), z = z, "", "", arglist, 
    TRUE, "tom"), "common.legend should be a logical value")
  
  # test legend
  expect_error(autoimage.xyz.setup(x = matrix(1, nrow = nr, ncol = nc), 
    y = matrix(1, nrow = nr, ncol = (nc + 1)), z = z, "", "", arglist, 
    TRUE, TRUE, c("horizontal", "horizontal")), "legend should be a single value")
})
