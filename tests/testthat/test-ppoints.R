test <- FALSE

test <- FALSE

if(test){
  setwd("~")
  pdf("test-plines.pdf")
  data(narccap)
  data(worldMapEnv, package = "maps")
  worldpoly <- maps::map("world", plot = FALSE)
  par(curpar)
  data(us.cities, package = "maps")
  usm <- head(us.cities)
  cityxy <- list(x = usm$long, y = usm$lat)
  pimage(lon, lat, z = tasmax[,,1], proj = "mercator", 
         points = cityxy, points.args = list(col = "orange", pch = 20))
  par(curpar)
  pimage(lon, lat, z = tasmax[,,1], proj = "mercator")
  ppoints(cityxy, proj = "mercator", col = "orange", pch = 20)
  dev.off()
}


