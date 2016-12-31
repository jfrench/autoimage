test <- FALSE

if (test) {
  setwd("~")
  pdf("test-ptext.pdf")
  data(narccap)
  data(us.cities, package = "maps")
  usm <- head(us.cities)
  cityxy <- list(x = usm$long, y = usm$lat)
  pimage(lon, lat, z = tasmax[, , 1], proj = "mercator", points = cityxy, 
    points.args = list(col = "orange", pch = 20))
  ptext(cityxy, labels = usm$name, proj = "mercator", col = "white", 
    pos = 3)
  dev.off()
}


TRUE
