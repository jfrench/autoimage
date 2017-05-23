context("ppolygon tests")

test <- FALSE

if (test) {
  setwd("~")
  pdf("test-ppolygon.pdf")
  data(narccap)
  data(copoly)
  data(us.cities, package = "maps")
  usm <- head(us.cities)
  pimage(lon, lat, z = tasmax[, , 1], proj = "mercator")
  ppolygon(copoly, proj = "mercator", col = "white")
  title("polygon over colorado")
  dev.off()
}