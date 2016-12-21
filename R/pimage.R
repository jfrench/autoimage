#' Display image for projected coordinates
#'
#' \code{pimage} plots an image for (potentially) projected locations.  
#' A color scale is automatically provided with the image.
#' The function is essentially an extension of the 
#' \code{\link[graphics]{image}} function and the \code{x}
#' and \code{y} locations can be irregularly-spaced
#' locations, sequences of increasing values for locations
#' on a regular grid, or matrices (with dimensions matching 
#' those of \code{z}) for locations on an irregular grid.  Functionality
#' for automatic projection is provided.
#'
#' If \code{x}, \code{y}, and \code{z} are numeric vectors of the
#' same length, then the \code{\link[akima]{interp}} function is
#' used to interpolate the locations onto a regular grid before
#' constructing the image.  This interpolation can be customized by
#' passing \code{interp.args} through \code{...}.  \code{interp.args}
#' should be a named list with component matching the non \code{x}, 
#' \code{y}, and \code{z} arguments of the 
#' \code{\link[akima]{interp}} function.
#'
#' When \code{proj != "none"}, the \code{\link[mapproj]{mapproject}}
#' function is used to project the \code{x} and \code{y} coordinates.
#' In that case, \code{proj} must correspond to one of the 
#' choices for the \code{projection} argument in the 
#' \code{\link[mapproj]{mapproject}} function.  Necessary arguments for 
#' \code{\link[mapproj]{mapproject}}
#' should be provided as a named list to the \code{proj.args} argument.
#' See Examples.
#' 
#' Valid options for \code{legend} are \code{"none"}, \code{"horizontal"},
#' and \code{"vertical"}.  If \code{legend = "none"}, then no color
#' scale is provided.  If \code{legend = "horizontal"}, then a color
#' scale is included under the image.  If \code{legend = "vertical"}, 
#' then a color scale is added to the right of the image.
#'
#' @param x,y Locations of grid points at which the values in \code{z} 
#' are measured.  The values must be finite and non-missing.  These 
#' arguments can be either vectors, in which case the values must be in (strictly) ascending order.  If an irregular grid is to be used, then \code{x} and \code{y} should be numeric matrices having the same number of rows and columns as \code{z}.  If these arguments are not provided, equally spaced values from 0 to 1 are used by default. If \code{x} is a list, its components \code{x$x} and \code{x$y} are used for \code{x} and \code{y}, respectively. If the list has component \code{x$z}, this is used for \code{z}.
#' @param z A numeric or logical matrix containing the values to be plotted (NAs are allowed). If multiple images are to be plotted, a numeric array can be provided instead.  The third dimension of the array indicates the number of images that should be plotted.  Note that \code{x} can be used instead of \code{z} for convenience.
#' @param legend A character string indicating where the color scale
#' should be placed.  The default is \code{"horizontal"}.  The other
#' valid options are \code{"none"} and \code{"vertical"}.
#' @param proj A character string indicating what projection should be
#' used for the included \code{x} and \code{y} coordinates.  The
#' default is \code{"none"}.  The other valid choices correspond to the
#' \code{"projection"} argument in the \code{\link[mapproj]{mapproject}} 
#' function, which is used for the projection.
#' @param proj.args A named list with arguments \code{parameters} and
#' \code{orientation} corresponding to the arguments of the same name
#' in the \code{\link[mapproj]{mapproject}} function.
#' @param lratio A numeric value indicating the ratio of the 
#' smaller dimension of the legend scale to the width of 
#' the image.  Default is \code{lratio = 0.2}.
#' @param map The name of map to draw on the image.  
#' Default is \code{"none"}.  Other options include
#' \code{"world"}, \code{"usa"}, \code{"state"}, \code{"county"}, 
#' \code{"france"}, \code{"nz"} (New Zealand), and \code{"world2"}, 
#' all from the \code{maps} package.
#' @param ... Additional arguments passed to the 
#' \code{\link[graphics]{image}} or \code{\link[fields]{poly.image}} 
#' functions.  e.g., \code{xlab}, \code{ylab}, \code{xlim}, \code{ylim},
#'  \code{zlim}, etc.  Additionally, arguments that can be used
#'  to further customize the plot, as described in Details and Examples.
#' @seealso \code{\link[graphics]{image}}, \code{\link[fields]{image.plot}}, \code{\link[graphics]{axis}}
#' @return NULL
#' @importFrom akima interp
#' @importFrom graphics axTicks axis box image layout par points lines mtext
#' @examples
#' curpar <- par(no.readonly = TRUE)
#' # image plot for data on an irregular grid
#' par(curpar) # reset graphics device
#' data(co, package = "gear")
#' pimage(co$longitude, co$latitude, co$Al)
#' 
#' # show observed locations on image,
#' # along with Colorado border
#' data(copoly)
#' copoints <- list(x = co$lon, y = co$lat)
#' par(curpar) # reset graphics device
#' pimage(co$longitude, co$latitude, co$Al, 
#'        lines = copoly, 
#'        lines.args = list(lwd = 2, col = "grey"),
#'        points = copoints, 
#'        points.args = list(pch = 21, bg = "white"),
#'        xlim = c(-109.1, -102),
#'        ylim = c(36.8, 41.1))
#' 
#' # image plot for data on irregular grid
#' # notice the poor axis labeling
#' par(curpar)
#' data(narccap)
#' pimage(lon, lat, tasmax[,,1], proj = "bonne",
#'        proj.args = list(parameters = 45),
#'        map = "world")
#' # same plot but customize axis labeling 
#' # need to extend horizontally-running axis lines
#' # farther to the west and east
#' # also need the vertically-running lines
#' # to run further north/sount
#' # will need manual adjusting depending on size
#' # of current device 
#' pimage(lon, lat, tasmax[,,1], proj = "bonne",
#'        proj.args = list(parameters = 45),
#'        map = "world", axes = FALSE)
#' paxes(proj = "bonne", 
#'       xlim = range(lon), ylim = range(lat), 
#'       xaxp = c(-200, 0, 10), 
#'       yaxp = c(-10, 80, 9))
#' 
#' # slightly different projection
#' pimage(lon, lat, tasmax[,,1], proj = "albers",
#'        proj.args = list(parameters = c(33, 45)),
#'        map = "world")
#'        
#' # same image with different arguments
#' pimage(lon, lat, tasmax[,,1], 
#'        legend = "vertical",
#'        proj = "bonne",
#'        proj.args = list(parameters = 45),
#'        map = "state",
#'        paxes.args = list(lty = 3),
#'        axis.args = list(col = "blue", col.axis = "blue"),
#'        col = fields::tim.colors(64),
#'        xlab = "longitude",
#'        ylab = "latitude",
#'        main = "temperature (K)")
#' par(curpar) # reset graphics device
#' @export
pimage <- function(x, y, z, legend = "horizontal", proj = "none", proj.args, 
                   lratio = 0.2, map = "none", ...){
  # determine current par values to restore later
  curpar <- par(no.readonly = TRUE)
  if(missing(x)) x <- NULL
  if(missing(y)) y <- NULL
  if(missing(z)) z <- NULL
  # obtain elements of ...
  arglist = list(...)
  tx <- ifelse(is.null(x), "", deparse(substitute(x)))
  ty <- ifelse(is.null(y), "", deparse(substitute(y)))
  if(proj != "none") arglist$asp <- 1
  xyz <- pimage.xyz.setup(x, y, z, tx, ty, arglist)
  # default proj.args
  if(missing(proj.args)) proj.args <- list()
  
  # check/setup arguments for pimage
  # print(xyz$arglist$legend.mar)
  object <- pimage.setup(xyz, legend, proj, proj.args, lratio, map)
  # print(object$legend.mar)
  if(legend != "none"){
    .legend.mar(object$legend.mar)
  }
  .legend.scale.args(object$legend.scale.args)
  # cat(paste(object$legend.mar,"\n"))
  # cat(paste(object$legend.scale.args,"\n"))
  
  if(legend == "none"){
    do.call(object$plotf, object$arglist)
  }else{
    # curmar <- par()$mar # current mar values
    autolayout(size = c(1, 1), legend = legend, lratio = lratio, show = FALSE, reverse = TRUE)
    autolegend()
    # par(mar = .legend.mar())
    # blank.plot()
    # autolegend()
    # par(mar = curpar$mar) # reset to original mar values
    do.call(object$plotf, object$arglist)
  }
  
  # plot axes, lines, points if desired
  if(object$axes){
    do.call("paxes", object$paxes.args)
  }
  if(!is.null(object$lines.args$x)){
    do.call("plines", object$lines.args)
  }
  if(!is.null(object$points.args$x)){
    f <- autoimage::ppoints
    do.call(f, object$points.args)
  }
  return(invisible(structure(object, class = "pimage")))
}

