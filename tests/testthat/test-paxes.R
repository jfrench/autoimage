context("paxes tests")

test <- FALSE

if (test) {
  setwd("~")
  pdf("test-paxes.pdf")
  data(narccap)
  pimage(lon, lat, tasmax[, , 1], proj = "mercator", axes = FALSE)
  paxes("mercator", xlim = range(lon), ylim = range(lat), col = "grey", 
    axis.args = list(col.axis = "blue", xat = c(-160, -100, -90, -80, 
      -20)))
  title("blue axis with unique x spacing, grey lines")
  dev.off()
}
TRUE
