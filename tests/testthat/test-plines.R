test <- FALSE

test <- FALSE

if(test){
  setwd("~")
  pdf("test-plines.pdf")
  data(narccap)
  data(worldMapEnv, package = "maps")
  worldpoly <- maps::map("world", plot = FALSE)
  par(curpar)
  pimage(lon, lat, z = tasmax[,,1], lines = worldpoly, main = "world poly", 
         proj = "mercator")
  par(curpar)
  pimage(lon, lat, z = tasmax[,,1], proj = "mercator", main = "world poly")
  plines(worldpoly, proj = "mercator")
  dev.off()
}


