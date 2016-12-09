data(narccap)
gimage(lon, lat, z = tasmax[,,1])

gimage <- function(x, y, z, projection = "none", legend = "horizontal", ...){
  if(missing(x)) x <- NULL
  if(missing(y)) y <- NULL
  if(missing(z)) z <- NULL  
  # obtain elements of ...
  arglist = list(...)
  # setup arguments for gimage
  object <- gplot.setup(x, y, z, projection, legend, arglist)

  if(legend == "none"){
    do.call(object$plotf, object$arglist)  
  }else{
    curmar <- par()$mar # current mar values
    autolayout(size = c(1, 1), legend = legend, mratio = 5, show = TRUE, reverse = TRUE)
    par(mar = legend.mar) # set mar for legend.scale
    do.call(legend.scale, legend.scale.args)
    par(mar = curmar) # reset to original mar values
    do.call(object$plotf, object$arglist)
  }
}

gplot.setup <- function(x, y, z, projection, legend, arglist){
  # sort out x, y, z, labels, etc.
  # This is a revision of the beginning of graphics::image
  
  # provide defaults to arglist, if necessary
  # get x and y labels, if necessary
  if(is.null(arglist$xlab)){
    arglist$xlab <- if (is.null(x)) "" else deparse(substitute(x))
  }
  if(is.null(arglist$ylab)){
    arglist$ylab <- if (is.null(y)) "" else deparse(substitute(y))
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
  if(length(projection) != 1){
    stop("project should be a single character string")
  }
  if(!is.character(projection)){
    stop("project should be a single character string")
  }
  # match legend argument
  legend <- try(match.arg(legend, c("none", "horizontal", "vertical")), silent = TRUE)
  if(length(legend) != 1){
    stop("legend should be a single logical value")
  }
  if(class(legend) == "try-error"){
    stop('invalid legend argument.  legend should be "none", "horizontal", or "vertical".')
  }
  
  # check colors  
  if(is.null(arglist$col)){
    arglist$col <- viridis::viridis(64)
  }
  
  # setup arguments for legend.scale function
  legend.mar <- arglist$legend.mar
  arglist$legend.mar <- NULL # remove non-graphical argument from arglist
  
  if(legend != "none"){
    legend.scale.args <- list()
    legend.scale.args$zlim <- arglist$zlim
    if(is.null(arglist$zlim)){
      arglist$zlim <- range(z, na.rm = TRUE)
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
  
  # setup interpolation arguments
  interp.args <- arglist$interp.args
  # remove non-graphical arguments, if provided
  arglist$interp.args <- NULL
  if(is.null(interp.args)){
    interp.args <- list()
  }

  project.args <- arglist$project.args
  arglist$project.args <- NULL
  if(is.null(project.args)) project.args <- list()
  
  if(! (length(x) == length(y))){
    stop("x and y do not have the same length and/or dimensions")
  }
  
  # Check for non-gridded points.  Interpolate onto regular grid.
  if(!is.matrix(x)){
    if(length(x) == length(z)){ 
      interp.args$x <- x
      interp.args$y <- y
      interp.args$z <- z
      ixyz <- do.call(akima::interp, interp.args)
      x <- ixyz$x
      y <- ixyz$y
      z <- ixyz$z
    }
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
                 legend.scale.args = legend.scale.args)
}

