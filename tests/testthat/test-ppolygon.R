test <- FALSE

if(test){
  setwd("~")
  pdf("test-polygon.pdf")
  data(narccap)
  data(copoly)
  par(curpar)
  data(us.cities, package = "maps")
  usm <- head(us.cities)
  pimage(lon, lat, z = tasmax[,,1], proj = "mercator")
  ppolygon(copoly, proj = "mercator", col = "white")
  dev.off()
}


