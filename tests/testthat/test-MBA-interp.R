context("MBA interp tests")

test <- FALSE

if (test) {
  setwd("~")
  pdf("test-MBA-interp.pdf")

data(LIDAR, package = "MBA")
autoimage(LIDAR[,1], LIDAR[,2], LIDAR[,3], legend = "v")
ggautoimage(LIDAR[,1], LIDAR[,2], LIDAR[,3])

autoimage(LIDAR[,1], LIDAR[,2], LIDAR[,3], 
          interp.args = list(no.X = 50, no.Y = 100))
autoimage(LIDAR[,1], LIDAR[,2], LIDAR[,3], 
          interp.args = list(nx = 50, ny = 100))
autoimage(LIDAR[,1], LIDAR[,2], LIDAR[,3], 
          interp.args = list(xo = seq(0, 1, len = 50), 
                             yo = seq(0, 1, len = 100)))

ggautoimage(LIDAR[,1], LIDAR[,2], LIDAR[,3], 
          interp.args = list(no.X = 50, no.Y = 100))
ggautoimage(LIDAR[,1], LIDAR[,2], LIDAR[,3], 
          interp.args = list(nx = 50, ny = 100))
ggautoimage(LIDAR[,1], LIDAR[,2], LIDAR[,3], 
          interp.args = list(xo = seq(0, 1, len = 50), 
                             yo = seq(0, 1, len = 100)))
dev.off()
}