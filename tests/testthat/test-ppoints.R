context("ppolygon tests")

test <- FALSE

if (test) {
  setwd("~")
  pdf("test-ppoints.pdf")
  data(narccap)
  data(worldMapEnv, package = "maps")
  worldpoly <- maps::map("world", plot = FALSE)
  data(us.cities, package = "maps")
  cap <- us.cities[us.cities$capital == 2, ]
  cityxy <- list(x = cap$long, y = cap$lat)
  pimage(lon, lat, z = tasmax[, , 1], proj = "mercator", points = cityxy, 
    points.args = list(col = "orange", pch = 20))
  plines(worldpoly, proj = "mercator")
  pimage(lon, lat, z = tasmax[, , 1], proj = "mercator")
  ppoints(cityxy, proj = "mercator", col = "orange", pch = 20)
  plines(worldpoly, proj = "mercator")
  dev.off()
}


TRUE
