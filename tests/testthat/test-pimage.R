test <- FALSE
# test <- TRUE

if(test){
  setwd("~")
  pdf("test-pimage.pdf")
  data(narccap)
  curpar <- par(no.readonly = TRUE)
  pimage(tasmax[,,1], main = "no axis labels")
  par(curpar)
  pimage(z = tasmax[,,1], main = "no axis labels")
  par(curpar)
  pimage(lon, lat, z = tasmax[,,1], legend = "none", main = "no legend, lon/lat labels")
  par(curpar)
  pimage(lon, lat, z = tasmax[,,1], main = "hor legend, lon/lat labels")
  par(curpar)
  pimage(lon, lat, z = tasmax[,,1], legend = "vertical", main = "vert legend, lon/lat labels")
  par(curpar)
  pimage(lon, lat, z = tasmax[,,1], main = "custom labels", xlab = "my xlab", ylab = "my ylab")
  
  data(worldMapEnv, package = "maps")
  worldpoly <- maps::map("world", plot = FALSE)
  par(curpar)
  pimage(lon, lat, z = tasmax[,,1], legend = "none", lines = worldpoly, main = "with world poly")
  par(curpar)
  pimage(lon, lat, z = tasmax[,,1], legend = "none", proj = "mercator", main = "with mercator proj")
  par(curpar)
  pimage(lon, lat, z = tasmax[,,1], legend = "none", proj = "bonne", 
         proj.args = list(parameters = 45), main = "with bonne projection and lines")
  plines(worldpoly, proj = "bonne")
  par(curpar)
  pimage(lon, lat, z = tasmax[,,1], legend = "none", proj = "bonne", 
         proj.args = list(parameters = 45), main = "with bonne projection no grid",
         paxes.args = list(grid = FALSE))
  par(curpar)
  pimage(lon, lat, z = tasmax[,,1], legend = "none", proj = "bonne", 
         proj.args = list(parameters = 45), main = "with bonne projection dashed grey grid",
         paxes.args = list(col = "grey", lty = 2))
  par(curpar)
  pimage(lon, lat, z = tasmax[,,1], legend = "none", proj = "mercator", 
         main = "bonne projection, thick grey lines",
         paxes.args = list(grid = FALSE),
         lines = worldpoly, lines.args = list(col = "grey", lwd = 3))
  par(curpar)
  data(us.cities, package = "maps")
  usm <- head(us.cities)
  cityxy <- list(x = usm$long, y = usm$lat)
  pimage(lon, lat, z = tasmax[,,1], legend = "none", proj = "mercator", 
         main = "mercator projection, solid orange points",
         paxes.args = list(grid = FALSE),
         points = cityxy, points.args = list(col = "orange", pch = 20))
  par(curpar)
  pimage(lon, lat, z = tasmax[,,1], legend = "vertical", lratio = 0.5,
         main = "wide vertical bar")
  par(curpar)
  pimage(lon, lat, z = tasmax[,,1], legend = "vertical", lratio = 0.5, 
         axis.args = list(col = "blue", col.axis = "blue", 
                          at = seq(280, 320, 20)),
         main = "blue legend text")
  par(curpar)
  pimage(lon, lat, z = tasmax[,,1], legend = "vertical", proj = "bonne", 
         proj.args = list(parameters = 45), xlim = c(-130, -60), 
         ylim = c(23, 50))
  plines(worldpoly, proj = "bonne")
  title("USA")
  
  par(curpar)
  pimage(lon, lat, z = tasmax[,,1], map = "world")
  title("narccap with worldpoly")
  
  par(curpar)
  pimage(lon, lat, z = tasmax[,,1], map = "county")
  title("narccap with counties")
  
  par(curpar)
  pimage(lon, lat, z = tasmax[,,1], map = "usa")
  title("narccap with usa border")
  
  par(curpar)
  pimage(lon, lat, z = tasmax[,,1], map = "state")
  title("narccap with state borders")
  
  par(curpar)
  pimage(lon + 360, lat, z = tasmax[,,1], map = "world2")
  title("narccap with world2 borders")
  
  par(curpar)
  pimage(lon + 90, lat, z = tasmax[,,1], map = "france")
  title("narccap with france borders")
  
  par(curpar)
  pimage(lon + 300, lat - 80, z = tasmax[,,1], map = "nz")
  title("narccap with new zealand borders")
  
  dev.off()
}


