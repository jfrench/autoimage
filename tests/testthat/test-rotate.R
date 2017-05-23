context("rotate tests")

# number of points to rotate
n <- 20
# random angle rotations
theta1 <- runif(1, 0, 2 * pi)
theta2 <- runif(1, 0, 2 * pi)

# random point to pivot around
pivot <- rnorm(2)

# create coords and ppp object
coords <- matrix(rnorm(2 * n), nrow = n)
win <- spatstat::owin(range(coords[, 1]), range(coords[, 2]))
ppcoords <- spatstat::ppp(coords[, 1], coords[, 2], window = win)

# rotation of theta1 around origin (0, 0)
rcoords <- rotate(coords, theta1)
# rotation of theta2 around random pivot point
rcoords2 <- rotate(coords, theta2, pivot)

# same rotations using spatstat package
rppcoords <- spatstat::rotate(ppcoords, theta1)
rppcoords2 <- spatstat::rotate(ppcoords, theta2, centre = pivot)

# make sure these are equal
test_that("autoimage::rotate matches spatstat::rotate", {
  expect_true(all.equal(rcoords, cbind(rppcoords$x, rppcoords$y)))
  expect_true(all.equal(rcoords2, cbind(rppcoords2$x, rppcoords2$y)))
})