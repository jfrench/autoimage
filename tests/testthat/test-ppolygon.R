test <- FALSE
# test <- TRUE

if(test){
  setwd("~")
  pdf("test-polygon.pdf")
  data(narccap)
  data(copoly)
  data(us.cities, package = "maps")
  usm <- head(us.cities)
  pimage(lon, lat, z = tasmax[,,1], proj = "mercator")
  ppolygon(copoly, proj = "mercator", col = "white")
  title("polygon over colorado")
  dev.off()
}


