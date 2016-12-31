# sorts out x, y, and z for autoimage function
autoimage.xyz.setup <- function(x, y, z, tx, ty, arglist, verbose, common.legend = FALSE, 
  legend = "none") {
  # sort out x, y, z, labels, etc.  Part of this is a revision of the
  # beginning of graphics::image
  if (is.null(x)) 
    tx <- ""
  if (is.null(y)) 
    ty <- ""
  arglist$mtext.args <- NULL
  
  # sanity checiking
  if (length(verbose) != 1) {
    stop("verbose must be a single logical value")
  }
  if (!is.logical(verbose)) {
    stop("verbose must be a single logical value")
  }
  if (length(common.legend) != 1) {
    stop("common.legend should be a single value")
  }
  if (!is.logical(common.legend)) {
    stop("common.legend should be a logical value")
  }
  if (length(legend) != 1) 
    stop("legend should be a single value")
  
  # set axis labels
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
  
  # checking x, y, z structure
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
  
  # make sure z is a matrix
  if (is.data.frame(z)) {
    z <- as.matrix(z)
  }
  
  # set plotting options
  if (common.legend) {
    if (is.null(arglist$zlim)) {
      arglist$zlim <- range(z, na.rm = TRUE)
    } else {
      if (is.list(arglist$zlim)) {
        stop("zlim should not be a list when common.legend = TRUE")
      }
      if (length(arglist$zlim) != 2) {
        stop("zlim should specify the minimum and maximum values of z")
      }
    }
  }
  
  if (is.null(arglist$legend.mar) & legend != "none") {
    arglist$legend.mar <- automar(legend)
  }
  
  # more x, y, z structure checking irregularly spaced coordinates
  if (!is.matrix(z) & !is.array(z)) {
    if (is.null(x) | is.null(y)) {
      stop("x and y must be specified when z is not a matrix or array")
    }
    if (!(length(x) == length(y))) {
      stop("x and y do not have the same length and/or dimensions")
    }
  } else {
    # z is matrix or array
    if (is.null(x)) 
      x <- seq.int(0, 1, length.out = nrow(z))
    if (is.null(y)) 
      y <- seq.int(0, 1, length.out = ncol(z))
    
    # if z is matrix or array, make sure x is a matrix of same dimension or
    # has correct spacing for regular grid or has irregularly spaced
    # coordinates of proper dimension
    
    if (!is.matrix(x)) {
      if (length(x) != nrow(z)) {
        stop("length(x) != nrow(z)")
      }
    }
    if (!is.matrix(y)) {
      if (length(y) != ncol(z) & length(y) != nrow(z)) {
        stop("length(y) != ncol(z) and length(y) != nrow(z)")
      }
    }
    if (is.matrix(x) | is.matrix(y)) {
      if (!is.matrix(x) | !is.matrix(y)) {
        stop("If x is a matrix, then y must be a matrix and vice versa")
      }
      if (!identical(dim(x), dim(y))) {
        stop("x and y must have the same dimensions if they are matrices")
      }
      if (nrow(x) != nrow(z) | ncol(x) != ncol(z)) {
        stop("nrow or ncol of x do not match nrow or ncol of z")
      }
    }
  }
  
  # determine third dimension of z and the type of plot that will be
  # constructed
  if (is.matrix(z)) {
    if (is.matrix(x)) {
      xyz.list <- vector("list", 1)
      arglist$x <- x
      arglist$y <- y
      arglist$z <- z
      xyz.list[[1]] <- arglist
      if (verbose) {
        message("note: a single irregular grid detected")
      }
    } else if (length(y) == ncol(z)) {
      xyz.list <- vector("list", 1)
      arglist$x <- x
      arglist$y <- y
      arglist$z <- z
      xyz.list[[1]] <- arglist
      if (verbose) {
        message("note: a single regular grid detected")
      }
    } else {
      xyz.list <- vector("list", ncol(z))
      # set main and zlim
      main <- rep(NULL, ncol(z))
      if (!is.null(arglist$main)) {
        if (ncol(z) != length(arglist$main) & length(arglist$main) != 
          1) {
          stop("length of main doesn not match number of images to construct")
        }
        if (length(arglist$main) == 1) {
          main <- rep(arglist$main, ncol(z))
        } else {
          main <- arglist$main
        }
      }
      zlim <- vector("list", ncol(z))
      if (!is.null(arglist$zlim)) {
        if (is.list(arglist$zlim)) {
          if (ncol(z) != length(arglist$zlim)) {
          stop("length of zlim does not match number of images to construct")
          }
          zlim <- arglist$zlim
        } else {
          zlim <- rep(list(arglist$zlim), ncol(z))
        }
      }
      
      for (i in seq_len(ncol(z))) {
        arglist0 <- arglist
        arglist0$main <- main[i]
        arglist0$zlim <- zlim[[i]]
        arglist0$x <- x
        arglist0$y <- y
        arglist0$z <- z[, i]
        xyz.list[[i]] <- arglist0
      }
      if (verbose) {
        message("note: sequence of irregularly-spaced points detected")
      }
    }
  } else if (is.array(z)) {
    # sequence of images
    n3 <- dim(z)[3]
    xyz.list <- vector("list", n3)
    main <- rep(NULL, n3)
    if (!is.null(arglist$main)) {
      if (n3 != length(arglist$main) & length(arglist$main) != 1) {
        stop("length of main does not match number of images to construct")
      }
      if (length(arglist$main) == 1) {
        main <- rep(arglist$main, ncol(z))
      } else {
        main <- arglist$main
      }
    }
    zlim <- vector("list", n3)
    if (!is.null(arglist$zlim)) {
      if (is.list(arglist$zlim)) {
        if (n3 != length(arglist$zlim)) {
          stop("length of zlim does not match number of images to construct")
        }
        zlim <- arglist$zlim
      } else {
        zlim <- rep(list(arglist$zlim), n3)
      }
    }
    
    for (i in seq_len(n3)) {
      arglist0 <- arglist
      arglist0$main <- main[i]
      arglist0$zlim <- zlim[[i]]
      arglist0$x <- x
      arglist0$y <- y
      arglist0$z <- z[, , i]
      xyz.list[[i]] <- arglist0
    }
    if (length(x) == nrow(z)) {
      # regular grid
      if (verbose) {
        message("note: sequence of regular grids detected")
      }
    } else {
      # irregular grid
      if (verbose) {
        message("note: sequence of irregular grids detected")
      }
    }
  } else {
    # single irregularly spaced
    xyz.list <- vector("list", 1)
    arglist$x <- x
    arglist$y <- y
    arglist$z <- z
    xyz.list[[1]] <- arglist
    if (verbose) {
      message("note: a single set of irregularly-spaced points detected")
    }
  }
  return(invisible(xyz.list))
}
TRUE
