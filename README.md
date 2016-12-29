autoimage README
================

[![Build Status](https://travis-ci.org/jpfrench81/autoimage.svg?branch=master)](https://travis-ci.org/jpfrench81/autoimage)

autoimage
---------

The goal of **autoimage** is to easily plot a sequence of images with corresponding color scales, i.e., a sequence of heatmaps, while also 
allowing for projections.

The package makes it easy to add lines and points to existing 
heatmaps, even for projected data.  The package allows for seamless 
plotting of gridded data on a non-regular grid or even data that is 
not on a grid.

Installation
------------

The release version of **autoimage** can be installed in R using the command `install.packages("autoimage")`.

The development version of `autoimage` can be installed in R using the command

    # install.packages("devtools")
    devtools::install_github("jpfrench81/autoimage")

Examples
--------

The most important functions are the `pimage` and `autoimage` functions. `pimage` creates a heat map with a color scale for data on a regular or irregular grid. It can also create an image for data that is not on a grid using interpolation.

The package also makes it easy to add lines and points to these images, with several common world maps being provided automatically through the `maps` package.

We illustrate some basic usage of the `pimage` function.

We begin by display images for data on an irregular grid, including national boundaries and legends.

``` r
# display some narccap data on an irregular grid
data(narccap)
# heat map for data on an irregular grid with national boundaries
pimage(lon, lat, tasmax[,,1], map = 'world', legend = "horizontal")
```

![](README_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
# same plot but with projection
pimage(lon, lat, tasmax[,,1], map = 'world', legend = "vertical",
       proj = 'bonne', parameters = 45)
```

![](README_files/figure-markdown_github/unnamed-chunk-1-2.png)

We next display an image for data that is not on a grid. Specifically, we create image plots for Colorado geochemical measurements. We also include the Colorado border in the image and the locations of the observed coordinates. Note that the `lines.args` and `points.args` arguments are used to customize the appearance of the lines and points.

``` r
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

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

The `autoimage` function generalizes the `pimage` function to allow for a sequence of images in a single graphic.

``` r
# plot irregularly gridded images with separate legends
# and usa border
autoimage(lon, lat, tasmax[,,1:2], common.legend = FALSE, map = 'usa')
```

    ## Warning in plot.window(...): "arglist" is not a graphical parameter

    ## Warning in plot.xy(xy, type, ...): "arglist" is not a graphical parameter

    ## Warning in title(...): "arglist" is not a graphical parameter

    ## Warning in plot.window(...): "arglist" is not a graphical parameter

    ## Warning in plot.xy(xy, type, ...): "arglist" is not a graphical parameter

    ## Warning in title(...): "arglist" is not a graphical parameter

![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
# with a common border and vertical scale
autoimage(lon, lat, tasmax[,,1:2], legend = "v", map = 'usa')
```

    ## Warning in plot.window(...): "arglist" is not a graphical parameter

    ## Warning in plot.xy(xy, type, ...): "arglist" is not a graphical parameter

    ## Warning in title(...): "arglist" is not a graphical parameter

    ## Warning in plot.window(...): "arglist" is not a graphical parameter

    ## Warning in plot.xy(xy, type, ...): "arglist" is not a graphical parameter

    ## Warning in title(...): "arglist" is not a graphical parameter

![](README_files/figure-markdown_github/unnamed-chunk-3-2.png)
