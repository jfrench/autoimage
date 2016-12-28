context("arg.check.ggautoimage sanity check")
test_that("arg.check.ggautoimage sanity check", {
  # check x, y, z
  # all vectors
  expect_error(arg.check.ggautoimage(factor(1:2), 1:2, 1:2, 
                                     factor(1:2), 
                                     "none", list(), 
                                     NULL, NULL, list()), 
               "x, y, and z must be vectors")
  expect_error(arg.check.ggautoimage(1:2, factor(1:2), 1:2, 
                                     factor(1:2), 
                                     "none", list(), 
                                     NULL, NULL, list()), 
               "x, y, and z must be vectors")
  expect_error(arg.check.ggautoimage(1:2, 1:2, factor(1:2),  
                                     factor(1:2), 
                                     "none", list(), 
                                     NULL, NULL, list()), 
               "x, y, and z must be vectors")
  # x, y, z length
  expect_error(arg.check.ggautoimage(1:3, 1:2, 1:2,  
                                     factor(1:2), 
                                     "none", list(), 
                                     NULL, NULL, list()), 
               "x, y, and z must have the same length")
  expect_error(arg.check.ggautoimage(1:2, 1:3, 1:2,  
                                     factor(1:2), 
                                     "none", list(), 
                                     NULL, NULL, list()), 
               "x, y, and z must have the same length")
  expect_error(arg.check.ggautoimage(1:2, 1:2, 1:3,  
                                     factor(1:2), 
                                     "none", list(), 
                                     NULL, NULL, list()), 
               "x, y, and z must have the same length")
  # x, y, z numeric
  expect_error(arg.check.ggautoimage(c("jim", "jim"), 1:2, 1:2,  
                                     factor(1:2), 
                                     "none", list(), 
                                     NULL, NULL, list()), 
               "x, y, and z must be numeric")
  expect_error(arg.check.ggautoimage(1:2, c("jim", "jim"), 1:2,  
                                     factor(1:2), 
                                     "none", list(), 
                                     NULL, NULL, list()), 
               "x, y, and z must be numeric")
  expect_error(arg.check.ggautoimage(1:2, 1:2, c("jim", "jim"),  
                                     factor(1:2), 
                                     "none", list(), 
                                     NULL, NULL, list()), 
               "x, y, and z must be numeric")
  # check factor
  expect_error(arg.check.ggautoimage(1:2, 1:2, 1:2,  
                                     1:3, 
                                     "none", list(), 
                                     NULL, NULL, list()), 
               "f must have the same length as x, y, and z")
  expect_error(arg.check.ggautoimage(1:2, 1:2, 1:2,  
                                     1:2, 
                                     "none", list(), 
                                     NULL, NULL, list()), 
               "f must be a factor")
  # proj
  expect_error(arg.check.ggautoimage(1:2, 1:2, 1:2,  
                                     factor(1:2), 
                                     c("none", "none"), list(), 
                                     NULL, NULL, list()), 
               "proj must be a single character string")
  expect_error(arg.check.ggautoimage(1:2, 1:2, 1:2,  
                                     factor(1:2), 
                                     1, list(), 
                                     NULL, NULL, list()), 
               "proj must be a single character string")
  # proj.args
  expect_error(arg.check.ggautoimage(1:2, 1:2, 1:2,  
                                     factor(1:2), 
                                     "none", 1, 
                                     NULL, NULL, list()), 
               "proj.args must be a list")
  # lines
  expect_error(arg.check.ggautoimage(1:2, 1:2, 1:2,  
                                     factor(1:2), 
                                     "none", list(), 
                                     1, NULL, list()), 
               "lines must be a list")
  expect_error(arg.check.ggautoimage(1:2, 1:2, 1:2,  
                                     factor(1:2), 
                                     "none", list(), 
                                     list(), NULL, list()), 
               "lines must have components x and y")
  expect_error(arg.check.ggautoimage(1:2, 1:2, 1:2,  
                                     factor(1:2), 
                                     "none", list(), 
                                     list(x = 1:2), NULL, list()), 
               "lines must have components x and y")
  expect_error(arg.check.ggautoimage(1:2, 1:2, 1:2,  
                                     factor(1:2), 
                                     "none", list(), 
                                     list(y = 1:2), NULL, list()), 
               "lines must have components x and y")
  expect_error(arg.check.ggautoimage(1:2, 1:2, 1:2,  
                                     factor(1:2), 
                                     "none", list(), 
                                     list(x = 1:3, y = 1:2), NULL, 
                                     list()))
  expect_error(arg.check.ggautoimage(1:2, 1:2, 1:2,  
                                     factor(1:2), 
                                     "none", list(), 
                                     list(x = factor(1:2), y = 1:2), NULL, 
                                     list()))
  # points
  expect_error(arg.check.ggautoimage(1:2, 1:2, 1:2,  
                                     factor(1:2), 
                                     "none", list(), 
                                     NULL, 
                                     1, 
                                     list()),
               "points must be a list")
  expect_error(arg.check.ggautoimage(1:2, 1:2, 1:2,  
                                     factor(1:2), 
                                     "none", list(), 
                                     NULL, 
                                     list(), 
                                     list()),
               "points must have components x and y")
  expect_error(arg.check.ggautoimage(1:2, 1:2, 1:2,  
                                     factor(1:2), 
                                     "none", list(), 
                                     NULL, 
                                     list(x = 1:2, y = 1:3), 
                                     list()))
  expect_error(arg.check.ggautoimage(1:2, 1:2, 1:2,  
                                     factor(1:2), 
                                     "none", list(), 
                                     NULL, 
                                     list(x = factor(1:2), y = 1:2), 
                                     list()))
  expect_error(arg.check.ggautoimage(1:2, 1:2, 1:2,  
                                     factor(1:2), 
                                     "none", list(), 
                                     NULL, 
                                     list(x = 1:2, y = 1:3), 
                                     list()))
  expect_error(arg.check.ggautoimage(1:2, 1:2, 1:2,  
                                     factor(1:2), 
                                     "none", list(), NULL, NULL,
                                     1),
               "interp.args must be a list")
})
