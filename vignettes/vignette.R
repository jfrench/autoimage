## ---- include=FALSE------------------------------------------------------
library(autoimage)

## ---- fig.height=5, fig.width=5------------------------------------------
data(narccap)
pimage(x = lon, y = lat, z = tasmax[,,1])

## ---- fig.height=5, fig.width=5------------------------------------------
pimage(x = lon, y = lat, z = tasmax[,,1], legend = "vertical")

