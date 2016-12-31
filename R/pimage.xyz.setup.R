# setup x, y, and z for pimage function
pimage.xyz.setup <- function(x, y, z, tx, ty, arglist) {
  # sort out x, y, z, labels, etc.  This is a revision of the beginning
  # of graphics::image
  if (is.null(x)) 
    tx <- ""
  if (is.null(y)) 
    ty <- ""
  
  if (is.null(arglist$xlab)) {
    if (is.null(z)) {
      arglist$xlab <- ""
    } else {
      arglist$xlab <- tx
    }
  }
  if (is.null(arglist$ylab)) {
    if (is.null(z)) {
      arglist$ylab <- ""
    } else {
      arglist$ylab <- ty
    }
  }
  
  if (is.null(z)) {
    if (!is.null(x)) {
      if (is.list(x)) {
        z <- x$z
        y <- x$y
        x <- x$x
      } else {
        if (is.null(dim(x))) 
          stop("argument must be matrix-like")
        z <- x
        x <- seq.int(0, 1, length.out = nrow(z))
        if (is.null(y)) 
          y <- seq.int(0, 1, length.out = ncol(z))
      }
    } else stop("no \"z\" matrix specified")
  } else if (is.list(x)) {
    y <- x$y
    x <- x$x
  }
  if (!is.matrix(z)) {
    if (is.null(x) | is.null(y)) {
      stop("x and y must be specified when z is not a matrix")
    }
    if (!(length(x) == length(y))) {
      stop("x and y do not have the same length and/or dimensions")
    }
  } else {
    if (is.null(x)) 
      x <- seq.int(0, 1, length.out = nrow(z))
    if (is.null(y)) 
      y <- seq.int(0, 1, length.out = ncol(z))
    if (!is.matrix(x)) {
      if (length(x) != nrow(z)) {
        stop("length(x) != nrow(z)")
      }
    }
    if (!is.matrix(y)) {
      if (length(y) != ncol(z)) {
        stop("length(y) != ncol(z)")
      }
    }
    if (is.matrix(x) | is.matrix(y)) {
      if (!is.matrix(x) | !is.matrix(y)) {
        stop("If x is a matrix, then y must be a matrix and vice versa")
      }
      if (!identical(dim(x), dim(z))) {
        stop("dim(x) should match dim(z)")
      }
      if (!identical(dim(y), dim(z))) {
        stop("dim(y) should match dim(z)")
      }
    }
  }
  return(list(x = x, y = y, z = z, arglist = arglist))
}
TRUE
