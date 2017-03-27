## ---- include=FALSE------------------------------------------------------
library(autoimage)
library(ggplot2)

## ------------------------------------------------------------------------
dates <- c("May 15, 2041", "May 16, 2041", "May 17, 2041", "May 18, 2041", "May 19, 2041")

## ---- fig.height=5, fig.width=5------------------------------------------
data(narccap)
autoimage(x = lon, y = lat, z = tasmax, main = dates)

## ---- fig.height=5, fig.width=5------------------------------------------
df1 <- data.frame(lon = c(lon[65:75, 50:60]), lat = c(lat[65:75, 50:60]), 
                  tasmax = c(tasmax[65:75, 50:60, 1]))
ggplot(df1, aes(x = lon, y = lat, z = tasmax, fill = tasmax)) + geom_tile()

## ------------------------------------------------------------------------
library(akima)
i1 <- interp(c(lon), c(lat), c(tasmax[,,1]), nx = 140, ny = 115)
i2 <- interp(c(lon), c(lat), c(tasmax[,,2]), nx = 140, ny = 115)
i3 <- interp(c(lon), c(lat), c(tasmax[,,3]), nx = 140, ny = 115)
i4 <- interp(c(lon), c(lat), c(tasmax[,,4]), nx = 140, ny = 115)
i5 <- interp(c(lon), c(lat), c(tasmax[,,5]), nx = 140, ny = 115)

## ------------------------------------------------------------------------
igrid <- expand.grid(i1$x, i1$y)
df <- data.frame(lon = igrid[,1], lat = igrid[,2], 
                 tasmax = c(c(i1$z), c(i2$z), c(i3$z), c(i4$z), c(i5$z)),
                 day = rep(dates, each = 16100)) 

## ---- fig.height=5, fig.width=5------------------------------------------
ggplot(df, aes(x = lon, y = lat, fill = tasmax)) + geom_tile() + 
  facet_wrap(~ day) 

## ---- fig.height=5, fig.width=5------------------------------------------
library(viridis)
ggplot(df, aes(x = lon, y = lat, fill = tasmax)) + geom_tile() + 
  facet_wrap(~ day) + scale_fill_viridis() + theme_bw()

## ---- fig.height=5, fig.width=5------------------------------------------
autoimage(x = lon, y = lat, z = tasmax, main = dates, 
          proj = "bonne", parameters = 45)
# ggplot(df, aes(x = lon, y = lat, fill = tasmax)) + geom_tile() + 
#   facet_wrap(~ day) + scale_fill_viridis() + theme_bw() + 
#   coord_map(projection = "bonne", parameters = 45)

## ---- fig.height=5, fig.width=5------------------------------------------
autoimage(x = lon, y = lat, z = tasmax, main = dates, map = "world")
world_df <- map_data("world")
ggplot() + geom_tile(aes(x = lon, y = lat, fill = tasmax), data = df) + 
  facet_wrap(~ day) + scale_fill_viridis() + theme_bw() + 
  geom_path(aes(x = long, y = lat, group = group), data = world_df) +
  xlim(c(-160, -34)) + ylim(c(20.5, 73.1))

