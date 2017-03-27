context("pimage tests")

test <- FALSE

if (test) {
  setwd("~")
  pdf("test-pimage.pdf")
  data(narccap)
  curpar <- par(no.readonly = TRUE)
  pimage(tasmax[, , 1], main = "no axis labels")
  par(curpar)
  pimage(z = tasmax[, , 1], main = "no axis labels")
  par(curpar)
  pimage(lon, lat, z = tasmax[, , 1], legend = "none", main = "no legend, lon/lat labels")
  par(curpar)
  pimage(lon, lat, z = tasmax[, , 1], main = "hor legend, lon/lat labels")
  par(curpar)
  pimage(lon, lat, z = tasmax[, , 1], legend = "vertical", main = "vert legend, lon/lat labels")
  par(curpar)
  pimage(lon, lat, z = tasmax[, , 1], main = "custom labels", xlab = "my xlab", 
    ylab = "my ylab")
  
  data(worldMapEnv, package = "maps")
  worldpoly <- maps::map("world", plot = FALSE)
  par(curpar)
  pimage(lon, lat, z = tasmax[, , 1], legend = "none", lines = worldpoly, 
    main = "with world poly")
  par(curpar)
  pimage(lon, lat, z = tasmax[, , 1], legend = "none", proj = "mercator", 
    main = "with mercator proj")
  par(curpar)
  pimage(lon, lat, z = tasmax[, , 1], legend = "none", proj = "bonne", 
    parameters = 45, main = "with bonne projection and lines")
  plines(worldpoly, proj = "bonne")
  par(curpar)
  pimage(lon, lat, z = tasmax[, , 1], legend = "none", proj = "bonne", 
    parameters = 45, main = "with bonne projection no grid", paxes.args = list(grid = FALSE))
  par(curpar)
  pimage(lon, lat, z = tasmax[, , 1], legend = "none", proj = "bonne", 
    parameters = 45, main = "with bonne projection dashed grey grid", 
    paxes.args = list(col = "grey", lty = 2))
  
  par(curpar)
  pimage(lon, lat, z = tasmax[, , 1], legend = "none", proj = "mercator", 
    main = "bonne projection, thick grey lines", paxes.args = list(grid = FALSE), 
    lines = worldpoly, lines.args = list(col = "grey", lwd = 3))
  
  par(curpar)
  data(us.cities, package = "maps")
  usm <- head(us.cities)
  cityxy <- list(x = usm$long, y = usm$lat)
  pimage(lon, lat, z = tasmax[, , 1], legend = "none", proj = "mercator", 
    main = "mercator projection, solid orange points", paxes.args = list(grid = FALSE), 
    points = cityxy, points.args = list(col = "orange", pch = 20))
  
  
  par(curpar)
  data(us.cities, package = "maps")
  usm <- head(us.cities)
  cityxy <- list(x = usm$long, y = usm$lat)
  pimage(lon, lat, z = tasmax[, , 1], legend = "none", proj = "mercator", 
         main = "mercator projection, orange numbered text", paxes.args = list(grid = FALSE), 
         text = cityxy, text.args = list(col = "orange"))
  
  par(curpar)
  pimage(lon, lat, z = tasmax[, , 1], legend = "vertical", lratio = 0.5, 
    main = "wide vertical bar")
  par(curpar)
  pimage(lon, lat, z = tasmax[, , 1], legend = "vertical", lratio = 0.5, 
    main = "blue legend text", legend.axis.args = list(col = "blue", 
      col.axis = "blue", at = seq(280, 320, 20)))
  
  par(curpar)
  pimage(lon, lat, z = tasmax[, , 1], legend = "vertical", proj = "bonne", 
    parameters = 45, xlim = c(-130, -60), ylim = c(23, 50))
  plines(worldpoly, proj = "bonne")
  title("USA")
  
  par(curpar)
  pimage(lon, lat, z = tasmax[, , 1], map = "world")
  title("narccap with worldpoly")
  
  par(curpar)
  pimage(lon, lat, z = tasmax[, , 1], map = "county")
  title("narccap with counties")
  
  par(curpar)
  pimage(lon, lat, z = tasmax[, , 1], map = "usa")
  title("narccap with usa border")
  
  par(curpar)
  pimage(lon, lat, z = tasmax[, , 1], map = "state")
  title("narccap with state borders")
  
  par(curpar)
  pimage(lon + 360, lat, z = tasmax[, , 1], map = "world2")
  title("narccap with world2 borders")
  
  par(curpar)
  pimage(lon + 90, lat, z = tasmax[, , 1], map = "france")
  title("narccap with france borders")
  
  par(curpar)
  pimage(lon + 300, lat - 80, z = tasmax[, , 1], map = "nz")
  title("narccap with new zealand borders")
  
  par(curpar)
  pimage(lon, lat, z = tasmax[, , 1], map = "lakes")
  title("narccap with lakes")
  
  par(curpar)
  pimage(lon + 100, lat, z = tasmax[, , 1], map = "italy")
  title("narccap with italy")
  
  par(curpar)
  pimage(lon, lat, z = tasmax[, , 1], axis.args = list(col.axis = "orange", 
    cex.axis = 0.5))
  title("orange, small axis")
  
  reset.par()
  pimage(lon, lat, z = tasmax[, , 1], proj = "mercator", axis.args = list(xat = c(-160, 
    -110, -80, -60, -40)))
  title("custom spaced x axis")
  
  reset.par()
  pimage(lon, lat, z = tasmax[, , 1], proj = "mercator", axis.args = list(yat = c(20, 
    40, 45, 70)), legend.axis.args = list(col = "blue", col.axis = "blue", 
    at = seq(280, 320, 20)))
  title("custom spaced y axis, legend axis")
  dev.off()
}


TRUE
