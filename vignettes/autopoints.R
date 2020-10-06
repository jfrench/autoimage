## ---- fig.height=5, fig.width=4-----------------------------------------------
library(autoimage)
data(co, package = "gear")
autopoints(co$longitude, co$latitude, co$Al, 
       xlab = "lon", ylab = "lat")

## ---- fig.height=5, fig.width=8-----------------------------------------------
autopoints(co$longitude, co$latitude, co[, c("Al", "Ca")], 
       xlab = "lon", ylab = "lat", common.legend = FALSE,
       main = c("Al", "Ca"))

## ---- fig.height=5, fig.width=8-----------------------------------------------
summary(co[, c("Al", "Ca")])
autopoints(co$longitude, co$latitude, co[, c("Al", "Ca")], 
       xlab = "lon", ylab = "lat", common.legend = FALSE,
       main = c("Al", "Ca"), zlim = list(c(0, 10), c(0, 22)))

## ---- fig.height=5, fig.width=8-----------------------------------------------
autopoints(co$longitude, co$latitude, co[, c("Al", "Ca")], 
       xlab = "lon", ylab = "lat", common.legend = FALSE,
       main = c("Al", "Ca"), 
       breaks = list(seq(0, 10, by = 2.5),
                     c(0, 2, 4, 6, 8, 22)))

## ---- fig.height=5, fig.width=8-----------------------------------------------
autopoints(co$longitude, co$latitude, co[, c("Al", "Ca")], 
       xlab = "lon", ylab = "lat",
       proj = "bonne", parameters = 40,
       paxes.args = list(col = "lightgrey"),
       axis.args = list(yat = 35:43, 
                        xat = -(110:101)),
       main = c("(a) Aluminum", "(b) Cadmium"),
       outer.title = "Geochemical measurements",
       mtext.args = list(col = "blue", cex = 2))

## ---- fig.height=5, fig.width=4-----------------------------------------------
autopoints(co$longitude, co$latitude, co$Al, 
       xlab = "lon", ylab = "lat", map = "county",
       lines.args = list(col = "magenta"))

## ---- fig.height=5, fig.width=4-----------------------------------------------
data(copoly)
autopoints(co$longitude, co$latitude, co$Al, 
       xlab = "lon", ylab = "lat", lines = copoly,
       lines.args = list(col = "orange", lwd = 3))

## ---- fig.height=4, fig.width=6-----------------------------------------------
autopoints(co$longitude, co$latitude, co$Al, legend = "v",
           xlab = "lon", ylab = "lat",
           col = colorspace::sequential_hcl(n = 4, palette = "Plasma"),
           breaks = c(0, 1, 2, 3, 8))

## ---- fig.height=4, fig.width=7-----------------------------------------------
citypoints = list(x = c(-104.98, -104.80), y = c(39.74, 38.85),
                  labels = c("Denver", "Colorado Springs"))
autopoints(co$lon, co$lat, co[,c("Al", "Ca")], common.legend = FALSE, 
           main = c("Aluminum", "Cadmium"), 
           points = citypoints,
           points.args = list(pch = 6, col = "magenta"),
           text = citypoints,
           text.args = list(pos = 3, col = "orange"),
           xlab = "lon", ylab = "lat")

## -----------------------------------------------------------------------------
# load world map
data(worldMapEnv, package = "maps")
# extract hawaii and alaskan borders
hiak <- maps::map("world", c("USA:Hawaii", "USA:Alaska"), 
                  plot = FALSE)
# load us city information
data(us.cities, package = "maps")
# extract colorado cities from us.cities
codf <- us.cities[us.cities$country.etc == "CO", ]
# select smaller subset of colorado cities
# extract capitals from us.cities
capdf <- us.cities[us.cities$capital == 2,]

## ---- fig.width=7, fig.height=5, hold=TRUE------------------------------------
# setup plotting area
autolayout(c(1, 2), legend = "h", common.legend = FALSE, outer = TRUE)
# create image of NARCCAP data.
# xlim is chosen so to include alaska and hawaii
# add grey state borders
# extend graticules (longitude/latitude grid lines)
pimage(lon, lat, tasmax[,,1], legend = "none",
       proj = "mercator", map = "state",
       xlim = c(-180, 20),
       axis.args = list(xat = seq(-175, -25, by = 25),
                        yat = seq(-10, 80, len = 10)),
       lines.args = list(col = "grey"))
# add hawaii and alaskan borders
plines(hiak, proj = "mercator", col = "grey")
# add state captials to image
ppoints(capdf$lon, capdf$lat, proj = "mercator", pch = 16)
# title image
title("tasmax for North America")
# add legend for plot
autolegend()
# load colorado geochemical data
data(co, package = "gear")
# create image for colorado aluminum measurements
# use bonne projection
# customize legend colors
# add grey county borders
# exclude longitude/latitude
heat_ppoints(co$lon, co$lat, co$Al, map = "county", legend = "none",
       proj = "bonne", parameters = 39, 
       paxes.args = list(grid = FALSE),
       col = cm.colors(5),
       lines.args = list(col = "grey"),
       xlab = "lon", ylab = "lat")
# add colorado city points to image
ppoints(codf$lon, codf$lat, pch = 16, proj = "bonne")
# add names of colorado cities to image
ptext(codf$lon, codf$lat, labels = codf$name, proj = "bonne", pos = 4)
# title plot
title("Colorado Aluminum levels (%)")
# add legend to current image
autolegend()
# add common title for plots
mtext("Two complicated maps", col = "purple", outer = TRUE, cex = 2)

