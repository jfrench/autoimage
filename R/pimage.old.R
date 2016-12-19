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
#' data(co, package = "gear")
#' pimage(co$longitude, co$latitude, co$Al, legend = "vertical", col = heat.colors(4))
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
#'        points.args = list(pch = 21, bg = "white"))
#' 
#' 
#' # image plot for data on irregular grid
#' par(curpar)
#' data(narccap)
#' pimage(lon, lat, tasmax[,,1], proj = "bonne",
#'        proj.args = list(parameters = 45),
#'        map = "world")
#'        
#' par(curpar)
#' # same image with different arguments
#' pimage(lon, lat, tasmax[,,1], 
#'        legend = "vertical",
#'        proj = "bonne",
#'        proj.args = list(parameters = 45),
#'        map = "state",
#'        paxes.args = list(col = "grey", lty = 3),
#'        axis.args = list(col = "blue", col.axis = "blue"),
#'        col = fields::tim.colors(64),
#'        xlab = "longitude",
#'        ylab = "latitude",
#'        main = "temperature (K)")
#'        
# no export
pimage.old <- function(x, y, z, legend = "horizontal", proj = "none", proj.args, 
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
  xyz <- pimage.xyz.setup(x, y, z, tx, ty, arglist)
  # default proj.args
  if(missing(proj.args)) proj.args <- list()
  
  # check/setup arguments for pimage
  object <- pimage.setup(xyz, legend, proj, proj.args, lratio, map)
  .pimage.legend(list(legend.mar = object$legend.mar,
                      legend.scale.args = object$legend.scale.args))
  
  if(legend == "none"){
    do.call(object$plotf, object$arglist)
  }else{
    curmar <- par()$mar # current mar values
    autolayout(size = c(1, 1), legend = legend, lratio = lratio, show = FALSE, reverse = TRUE)
    par(mar = object$legend.mar) # set mar for legend.scale
    do.call(legend.scale, object$legend.scale.args)
    par(mar = curmar) # reset to original mar values
    do.call(object$plotf, object$arglist)
  }
  
  # plot axes and grid lines, if desired
  if(object$axes){
    do.call("paxes", object$paxes.args)
  }
  
  if(!is.null(object$lines.args$x)){
    do.call("plines", object$lines.args)
  }
  if(!is.null(object$points.args$x)){
    do.call("ppoints", object$points.args)
  }
  return(invisible(structure(object, class = "pimage")))
}

pimage.xyz.setup <- function(x, y, z, tx, ty, arglist){
  # sort out x, y, z, labels, etc.
  # This is a revision of the beginning of graphics::image
  if(is.null(arglist$xlab)){
    if(is.null(z)){
      arglist$xlab <- ""
    }else{
      arglist$xlab <- tx
    }
  }
  if(is.null(arglist$ylab)){
    if(is.null(z)){
      arglist$ylab <- ""      
    }else{
      arglist$ylab <- ty
    }
  }

  if (is.null(z)) {
    if (!is.null(x)) {
      if (is.list(x)) {
        z <- x$z; y <- x$y; x <- x$x
      } else {
        if(is.null(dim(x)))
          stop("argument must be matrix-like")
        z <- x
        x <- seq.int(0, 1, length.out = nrow(z))
        if(is.null(y)) y <- seq.int(0, 1, length.out = ncol(z))
      }
    } else stop("no 'z' matrix specified")
  } else if (is.list(x)) {
    y <- x$y
    x <- x$x
  } 
  if(!is.matrix(z)){
    if(is.null(x) | is.null(y)){
      stop("x and y must be specified when z is not a matrix")
    }
    if(! (length(x) == length(y))){
      stop("x and y do not have the same length and/or dimensions")
    }
  }else{
    if(is.null(x)) x <- seq.int(0, 1, length.out = nrow(z))
    if(is.null(y)) y <- seq.int(0, 1, length.out = ncol(z))
    if(!is.matrix(x)){
      if(length(x) != nrow(z)){
        stop("length(x) != nrow(z)")
      }
    }
    if(!is.matrix(y)){
      if(length(y) != ncol(z)){
        stop("length(y) != ncol(z)")
      }
    }
    if(is.matrix(x) | is.matrix(y)){
      if(!is.matrix(x) | !is.matrix(y)){
        stop("If x is a matrix, then y must be a matrix and vice versa")
      }
      if(!identical(dim(x), dim(z))){
        stop("dim(x) should match dim(z)")
      }
      if(!identical(dim(y), dim(z))){
        stop("dim(y) should match dim(z)")
      }
    }
  }
  return(list(x = x, y = y, z = z, arglist = arglist))
}