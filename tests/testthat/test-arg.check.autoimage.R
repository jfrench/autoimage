context("arg.check.autoimage tests")

tx <- ""
ty <- ""
arglist <- list()
nr <- nrow(tasmax)
nc <- ncol(tasmax)
data(co, package = "gear")
test_that("autoimage.xyz.setup detects correct type of problem", {
  # check common.legend
  expect_error(arg.check.autoimage(c(TRUE, TRUE)), "common.legend should be a logical value")
  expect_error(arg.check.autoimage("jim"), "common.legend should be a logical value")
  # check size
  expect_error(arg.check.autoimage(TRUE, size = 1, ng = 1), "size should be a vector of length 2")
  expect_error(arg.check.autoimage(TRUE, size = c(1, 1), ng = 2), "size is not large enough to hold all plots")
  expect_error(arg.check.autoimage(TRUE, size = c("jim", "jim"), ng = 2), 
    "size should be a numeric vector")
  # check outer.title
  expect_error(arg.check.autoimage(TRUE, outer.title = letters[1:2]), 
    "outer.title should have length 1")
  # check mtext.args
  expect_error(arg.check.autoimage(TRUE, mtext.args = "tom"), "mtext.args should be a list")
})


TRUE
