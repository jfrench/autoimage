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
#' # image plot for data on an irregular grid
#' data(co, package = "gear")
#' pimage(co$longitude, co$latitude, co$Al)
#' @export
pimage <- function(x, y, z, legend = "horizontal", proj = "none", proj.args, 
                   lratio = 0.2, ...){
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
  object <- pimage.setup(xyz, legend, proj, proj.args, lratio)

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

pimage.setup <- function(xyz, legend, proj, proj.args, lratio){
  x <- xyz$x
  y <- xyz$y
  z <- xyz$z
  arglist <- xyz$arglist
  if(length(proj) != 1){
    stop("proj should be a single character string")
  }
  if(!is.character(proj)){
    stop("proj should be a single character string")
  }
  # match legend argument
  legend <- try(match.arg(legend, c("none", "horizontal", "vertical")), silent = TRUE)
  if(length(legend) != 1){
    stop("legend should be a single logical value")
  }
  if(class(legend) == "try-error"){
    stop('invalid legend argument.  legend should be "none", "horizontal", or "vertical".')
  }
  if(length(lratio) != 1){
    stop("lratio should be a positive number")
  }
  if(!is.numeric(lratio)){
    stop("lratio should be a positive number")
  }
  if(lratio <= 0){
    stop("lratio should be a positive number")
  }
  
  # check colors  
  if(is.null(arglist$col)){
    arglist$col <- viridis::viridis(64)
  }
  
  # setup arguments for legend.scale function
  legend.mar <- arglist$legend.mar
  arglist$legend.mar <- NULL # remove non-graphical argument from arglist
  
  legend.scale.args <- list()
  if(legend != "none"){
    legend.scale.args$zlim <- arglist$zlim
    if(is.null(arglist$zlim)){
      arglist$zlim <- range(z, na.rm = TRUE)
      legend.scale.args$zlim <- arglist$zlim
    }
    legend.scale.args$col <- arglist$col
    legend.scale.args$horizontal <- ifelse(legend == "horizontal", TRUE, FALSE)
    legend.scale.args$breaks <- arglist$breaks
    legend.scale.args$axis.args <- arglist$axis.args
    # remove non-graphical argument from arglist
    arglist$axis.args <- NULL 
    
    if(is.null(legend.mar)){
      legend.mar = par()$mar
      if(legend == "horizontal"){
        legend.mar[3] = 0
        legend.mar[1] = 3.1
      }else{
        legend.mar[2] = 0
        legend.mar[4] = 3.1
      }
    }
  }
  
  # check if there are points to plot
  points <- arglist$points
  arglist$points <- NULL
  if(!is.null(points)){
    if(!is.list(points)){
      stop("points must be a list with vectors x and y")
    }
    if(is.null(points$x) | is.null(points$y)){
      stop("points must be a list with vectors x and y")
    }
    if(length(points$x) != length(points$y)){
      stop("The x and y vectors in points should have the same length")
    }
  }
  points.args <- arglist$points.args
  arglist$points.args <- NULL
  points.args$proj <- proj
  points.args$x <- points
  
  # check if there are lines to plot
  lines <- arglist$lines
  arglist$lines <- NULL
  if(!is.null(lines)){
    if(!is.list(lines)){
      stop("lines must be a list with vectors x and y")
    }
    if(is.null(lines$x) | is.null(lines$y)){
      stop("lines must be a list with vectors x and y")
    }
    if(length(lines$x) != length(lines$y)){
      stop("The x and y vectors in lines should have he same length")
    }
  }
  lines.args <- arglist$lines.args
  arglist$lines.args <- NULL
  lines.args$proj <- proj
  lines.args$x <- lines
  
  # setup interpolation arguments
  interp.args <- arglist$interp.args
  # remove non-graphical arguments, if provided
  arglist$interp.args <- NULL
  if(is.null(interp.args)){
    interp.args <- list()
  }
  
  # Check for non-gridded points.  Interpolate onto regular grid.
  if(!is.matrix(x)){
    if(length(x) == length(z)){ 
      interp.args$x <- x
      interp.args$y <- y
      interp.args$z <- z
      interpf <- akima::interp
      ixyz <- do.call(interpf, interp.args)
      x <- ixyz$x
      y <- ixyz$y
      z <- ixyz$z
    }
  }
  
  paxes.args <- arglist$paxes.args
  if(!is.null(arglist$paxes.args)){
    arglist$paxes.args <- NULL
  }
  if(is.null(arglist$xlim)){
    arglist$xlim <- range(x)
  }
  paxes.args$xlim <- arglist$xlim
  if(is.null(arglist$ylim)){
    arglist$ylim <- range(y)
  }
  paxes.args$ylim <- arglist$ylim
  paxes.args$xaxp <- arglist$xaxp
  paxes.args$yaxp <- arglist$yaxp
  paxes.args$proj <- proj
  
  if(is.null(arglist$axes)){
    axes <- TRUE
  }else{
    axes <- arglist$axes
  }
  # will plot axes manually, if necessary
  arglist$axes <- FALSE
  
  if(proj != "none"){
    if(!is.list(proj.args)) stop("proj.args should be a list")
    if(!is.matrix(x)){
      x = matrix(x, nrow = dim(z)[1], ncol = dim(z)[2])
    }
    if(!is.matrix(y)){
      y = matrix(y, nrow = dim(z)[1], ncol = dim(z)[2], byrow = TRUE)
    }
    xv <- c(x)
    yv <- c(y)
    
    which.in <- which(xv >= arglist$xlim[1] &
                        xv <= arglist$xlim[2] & 
                        yv >= arglist$ylim[1] &
                        yv <= arglist$ylim[2])
    
    
    
    projectxy = mapproj::mapproject(c(x), c(y), projection = proj, 
                                    parameters = proj.args$parameters, 
                                    orientation = proj.args$orientation)
    x = matrix(projectxy$x, nrow = nrow(x))
    y = matrix(projectxy$y, nrow = nrow(y))
    
    # allxlim <- rep(arglist$xlim, each = 2)
    # allylim <- rep(arglist$ylim, times = 2)
    # 
    # projectlim <- mapproj::mapproject(allxlim, allylim)
    arglist$xlim <- range(x[which.in])
    arglist$ylim <- range(y[which.in])
    
    # projectat <- mapproj::mapproject(arglist$xat, arglist$ylim)
    # arglist$xlim <- projectlim$x
    # arglist$ylim <- projectlim$y
  }
  
  # is the grid a regular grid
  regular <- ifelse(length(x) != nrow(z), FALSE, TRUE)
  # decide plotting function accordingly
  plotf <- fields::poly.image
  if(regular) plotf <- image
  
  arglist$x <- x
  arglist$y <- y
  arglist$z <- z
  
  object <- list(plotf = plotf, 
                 arglist = arglist, 
                 legend = legend, 
                 legend.scale.args = legend.scale.args,
                 legend.mar = legend.mar, 
                 proj = proj,
                 points = points,
                 points.args = points.args,
                 lines = lines,
                 lines.args = lines.args,
                 axes = axes,
                 paxes.args = paxes.args)
  object
}


