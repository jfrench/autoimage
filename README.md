[![Build Status](https://travis-ci.org/jpfrench81/autoimage.svg?branch=master)](https://travis-ci.org/jpfrench81/autoimage)

# autoimage

The goal of `autoimage` is to easily plot a sequence of images with corresponding color scales, i.e., a sequence of heatmaps.

The function provides straightforward functions for projecting longitude/latitude coordinates and adding polygons.  The package allows for seamless plotting of gridded data on a non-regular grid.

## Installation

The release version of `autoimage` can be installed in R using the command `install.packages("autoimage")`.

The development version of `autoimage` can be installed in R using the command  
```R
# install.packages("devtools")
devtools::install_github("jpfrench81/autoimage")
```
=======

## Example

The most important functions are the `pimage` and `autoimage` functions.
`pimage` creates a heat map with a color scale for data on
a regular or irregular grid.  It an also create an image for
data that is not on a grid using interpolation.

The package also makes it easy to add lines and points to these images, 
with several common world maps being provided automatically through
the `maps` package.

We illustrate some basic usage of the `pimage` function first.

```R
# display some narccap data on an irregular grid
data(narccap)
# heat map for data on an irregular grid with national boundaries
pimage(lon, lat, tasmax[,,1], map = 'world', legend = "horizontal")
# same plot but with projection
pimage(lon, lat, tasmax[,,1], map = 'world', legend = "vertical",
       proj = 'bonne', proj.args = list(parameters = 45))

# colorado data not on a grid
# show observed locations on image,
# along with Colorado border
data(co, package = 'gear') # load colorado geochemical measurements
data(copoly) # load colorado borders
# create list with observed points
copoints <- list(x = co$lon, y = co$lat) 
reset.par() # reset graphics device
pimage(co$longitude, co$latitude, co$Al, 
       lines = copoly, 
       lines.args = list(lwd = 2, col = 'grey'),
       points = copoints, 
       points.args = list(pch = 21, bg = 'white'),
       xlim = c(-109.1, -102),
       ylim = c(36.8, 41.1))
```

The `autoimage` function generalized the `pimage` function to 
allow for a sequence of images.

```
# plot irregularly gridded images with separate legends
# and usa border
autoimage(lon, lat, tasmax[,,1:2], common.legend = FALSE, map = 'usa')
# with a common border and vertical scale
autoimage(lon, lat, tasmax[,,1:2], legend = "v", map = 'usa')
```

