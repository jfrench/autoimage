#' Automatic facetting of multiple projected images
#'
#' \code{autoimage} plots a sequence of images (with possibly 
#' projected coordinates) while also automatically plotting a 
#' color scale matching the image colors to the values of \code{z}.  
#' Many options are available for legend customization.  The coordinates
#' can be irregularly spaced, on a regular grid, or on an irregular 
#' grid.  \code{z} can be a numeric vector, matrix, or array, depending
#' on the context.
#' 
#' The \code{\link[mapproj]{mapproject}} function is used to project 
#' the \code{x} and \code{y} coordinates when \code{proj != "none"}.
#'
#' If multiple images are to be plotted (i.e., if \code{z} is an array), 
#' then the \code{main} argument can be a vector with length matching 
#' \code{dim(z)[3]}, and each successive element of the vector will 
#' be used to add a title to each successive image plotted.  
#' See the Examples.
#'
#' Additionally, if \code{common.legend = FALSE}, then separate limits 
#' for the z-axis of each image can be provided as a list.  
#' Specifically, if \code{dim(z)[3] == k}, then \code{zlim} should 
#' be a list of length \code{k}, and each element of the list should 
#' be a 2-dimensional vector providing the lower and upper limit, 
#' respectively, of the legend for each image.  Alternatively, if 
#' \code{zlim} is a list of length \code{k}, then \code{common.legend}
#' is set to \code{FALSE}. 
#' 
#' The range of \code{zlim} is cut into \eqn{n} partitions, 
#' where \code{n} is the length of \code{col}.
#'
#' Note that the more images that are plotted simulataneously, 
#' the smaller one typically wants \code{lratio} to be.
#'
#' The multiple plots are constructed using the 
#' \code{\link[autoimage]{autolayout}} function, which 
#' is incompatible with the \code{mfrow} and \code{mfcol} arguments 
#' in the \code{\link[graphics]{par}} function and is also 
#' incompatible with the \code{\link[graphics]{split.screen}} function.
#' 
#' The \code{mtext.args} argument can be passed through \code{...} 
#' in order to customize the outer title.  This should be a named
#' list with components matching the arguments of 
#' \code{\link[graphics]{mtext}}.
#' 
#' Lines can be added to each image by passing the \code{lines} 
#' argument through \code{...}.  In that case, \code{lines} should be
#' a list with components \code{x} and \code{y} specifying the
#' locations to draw the lines.  The appearance of the plotted lines
#' can be customized by passing a named list called \code{lines.args}
#' through \code{...}. The components of \code{lines.args} should match
#' the arguments of \code{\link[graphics]{lines}}.  See Examples.
#' 
#' Points can be added to each image by passing the \code{points} 
#' argument through \code{...}.  In that case, \code{points} should be
#' a list with components \code{x} and \code{y} specifying the
#' locations to draw the points.  The appearance of the plotted points
#' can be customized by passing a named list called \code{points.args}
#' through \code{...}. The components of \code{points.args} should match
#' the components of \code{\link[graphics]{points}}.  See Examples.
#' 
#' Text can be added to each image by passing the \code{text} 
#' argument through \code{...}.  In that case, \code{text} should be
#' a list with components \code{x} and \code{y} specifying the
#' locations to draw the text, and \code{labels}, a component
#' specifying the actual text to write.  The appearance of the plotted text
#' can be customized by passing a named list called \code{text.args}
#' through \code{...}. The components of \code{text.args} should match
#' the components of \code{\link[graphics]{text}}.  See Examples.
#'  
#' The legend scale can be modified by passing \code{legend.axis.args}
#' through \code{...}.  The argument should be a named list
#' corresponding to the arguments of the \code{\link[graphics]{axis}}
#' function.  See Examples.
#' 
#' The image axes can be modified by passing \code{axis.args}
#' through \code{...}.  The argument should be a named list
#' corresponding to the arguments of the \code{\link[graphics]{axis}}
#' function.  The exception to this is that arguments \code{xat} 
#' and \code{yat} can be specified (instead of \code{at}) to specify
#' the location of the x and y ticks.  If \code{xat} or \code{yat}
#' are specified, then this overrides the \code{xaxt} and \code{yaxt}
#' arguments, respectively.  See the \code{\link[autoimage]{paxes}}
#' function to see how \code{axis.args can be used.}
#' 
#' The legend margin can be customized by passing \code{legend.mar}
#' to \code{pimage} through \code{...}.  This should be a numeric
#' vector indicating the margins of the legend, identical to how 
#' \code{par("mar")} is specified.
#' 
#' The various options of the labeling, axes, and legend are largely
#' independent.  e.g., passing \code{col.axis} through \code{...} 
#' will not affect the axis unless it is passed as part of the 
#' named list \code{axis.args}.  However, one can set the various
#' \code{par} options prior to plotting to simultaneously
#' affect the appearance of multiple aspects of the plot.  See 
#' Examples for \code{\link[autoimage]{pimage}}.  After plotting, 
#' \code{reset.par()} can be used to reset 
#' the graphics device options to their default values. 
#' 
#' @inheritParams pimage
#' @inheritParams autolayout
#' @param outer.title A title related to all of the images that is plotted in the outer margin of the figure.
#' @param ... Additional arguments passed to the \code{\link[graphics]{image}} or 
#' \code{\link[fields]{poly.image}} functions.  e.g., \code{xlab}, \code{ylab}, 
#' \code{xlim}, \code{ylim}, \code{zlim}, etc.
#' @seealso \code{\link[autoimage]{pimage}}
#' @return NULL
#' @examples
#' data(narccap)
#' # restructure data for 2 images
#' tasmax2 <- tasmax[,,1:2]
#' 
#' # plot irregularly gridded images with separate legends
#' # and usa border
#' autoimage(lon, lat, tasmax2, common.legend = FALSE, map = "usa")
#' 
#' # plot irregularly gridded images with common legend and world lines
#' # customize world lines
#' # add and customize title
#' autoimage(lon, lat, tasmax2, map = "world", 
#'           lines.args = list(col = "white", lwd = 2),
#'           outer.title = "Maximum Daily Surface Air Temperature (K)",
#'           mtext.args = list(col = "blue", cex = 2))
#' 
#' # plot irregularly-spaced responsed as images with separate legends
#' # and county borders.  Add observed data locations with custom point
#' # options.  Add text at locations of Denver and Colorado Springs.
#' data(co, package = "gear")
#' autoimage(co$lon, co$lat, co[,c("Al", "Ca")], common.legend = FALSE, 
#'           map = "county", main = c("Aluminum", "Cadmium"),
#'           points = list(x = co$lon, y = co$lat),
#'           points.args = list(pch = 20, col = "white"),
#'           text = list(x = c(-104.98, -104.80), y = c(39.74, 38.85), 
#'                       labels = c("Denver", "Colorado Springs")),
#'           text.args = list(col = "red"))
#' 
#' # customize margins and lratio for large plot
#' # also use projection
#' # specify manual lines (though in this case it is the same as using 
#' # map = "world")
#' data(worldMapEnv, package = "maps")
#' worldpoly <- maps::map("world", plot = FALSE)
#' par(mar = c(1.1, 4.1, 2.1, 1.1))
#' autoimage(lon, lat, tasmax, lines = worldpoly, 
#'           proj = "bonne", parameters = 40,
#'           main = c("day 1", "day 2", "day 3", "day 4", "day 5"),
#'           ylab = "",
#'           axes = FALSE,
#'           lratio = 0.5)
#' @export
autoimage <- function(x, y, z, legend = "horizontal", proj = "none", parameters, 
  orientation, lratio = 0.2, common.legend = TRUE, map = "none", size, 
  outer.title, ...) {
  # obtain elements of ...
  arglist <- list(...)
  mtext.args <- arglist$mtext.args
  
  # make compatible with old version
  if (!is.logical(legend)) {
    legend <- match.arg(legend, c("none", "horizontal", "vertical"))
  }
  # attempt to match deprecated arguments
  argmatch <- autoimage.match.old.args(legend, proj, list(), lratio, 
    arglist)
  legend <- argmatch$legend
  lratio <- argmatch$lratio
  arglist <- argmatch$arglist
  
  # set default for missing arguments
  if (missing(x)) 
    x <- NULL
  if (missing(y)) 
    y <- NULL
  if (missing(z)) 
    z <- NULL
  if (missing(outer.title)) 
    outer.title <- NULL
  if (missing(parameters)) 
    parameters <- NULL
  if (missing(orientation)) 
    orientation <- NULL
  # deparse label names tx <- ifelse(is.null(x), "",
  # deparse(substitute(x))) ty <- ifelse(is.null(y), "",
  # deparse(substitute(y)))
  
  verbose <- FALSE  # some debugging stuff
  # setup x, y, z information
  xyz.list <- autoimage.xyz.setup(x, y, z, deparse(substitute(x)), deparse(substitute(x)), 
    arglist, verbose, common.legend, legend)
  ng <- length(xyz.list)  # number of grids
  # additional argument checking
  if (missing(size)) 
    size <- autosize(length(xyz.list))
  # change common.legend if zlim is a list
  if (!is.null(arglist$zlim)) {
    if (is.list(arglist$zlim)) 
      common.legend <- FALSE
  }
  # check other argument, specify outer arguments
  outer.args <- arg.check.autoimage(common.legend, size, outer.title, 
    ng, mtext.args)
  
  curpar <- par(no.readonly = TRUE)
  curmar <- curpar$mar  # current mar values
  autolayout(size, legend = legend, common.legend = common.legend, lratio = lratio, 
    outer = outer.args$outer, show = FALSE, reverse = FALSE)
  for (i in seq_along(xyz.list)) {
    par(mar = curmar)
    arglisti <- xyz.list[[i]]
    arglisti$legend <- "none"
    arglisti$proj <- proj
    arglisti$parameters <- parameters
    arglisti$orientation <- orientation
    arglisti$map <- map
    do.call("pimage", arglisti)
    if (!common.legend & legend != "none") {
      autolegend()
    }
  }
  
  deficit <- prod(size) - ng
  if (!common.legend & legend != "none") 
    deficit <- 2 * deficit
  for (i in seq_len(deficit)) {
    blank.plot()
  }
  if (common.legend & legend != "none") {
    autolegend()
  }
  
  # plot outer title, if necessary
  if (outer.args$outer) {
    do.call("mtext", outer.args$mtext.args)
  }
  
  # restore previous par() settings
  on.exit(par(curpar))
}

# check arguments of functions
arg.check.autoimage <- function(common.legend, size = c(1, 1), outer.title = NULL, 
  ng = 1, mtext.args = NULL) {
  if (length(common.legend) != 1) 
    stop("common.legend should be a logical value")
  if (!is.logical(common.legend)) 
    stop("common.legend should be a logical value")
  if (length(size) != 2) 
    stop("size should be a vector of length 2")
  if (!is.numeric(size)) 
    stop("size should be a numeric vector")
  if (prod(size) < ng) 
    stop("size is not large enough to hold all plots")
  if (!is.null(outer.title)) {
    if (length(outer.title) != 1) 
      stop("outer.title should have length 1")
    if (!is.character(outer.title)) 
      stop("outer.title should be a character string")
  }
  if (is.null(mtext.args)) 
    mtext.args <- list()
  if (!is.list(mtext.args)) 
    stop("mtext.args should be a list")
  outer <- FALSE
  if (!is.null(outer.title)) {
    outer <- TRUE
    mtext.args$text <- outer.title
    mtext.args$outer <- TRUE
  }
  return(list(outer = outer, mtext.args = mtext.args))
}

# tries to match argument from old version of package
# and update for new version.
autoimage.match.old.args <- function(legend = "horizontal", proj = "none", 
                                     proj.args, lratio = 0.2, arglist) {
  if (length(legend) != 1) {
    stop("legend should have length 1")
  }
  if (is.logical(legend)) {
    warning("autoimage has been updated.  Trying to translate arguments to current version")
    if (legend) {
      if (is.null(arglist$horizontal)) {
        legend <- "horizontal"
      } else if (arglist$horizontal) {
        legend <- "horizontal"
      } else {
        legend <- "vertical"
      }
      arglist$horizontal <- NULL
    } else {
      legend <- "none"
    }
  }
  if (length(proj) != 1) {
    stop("proj should be a single character string")
  }
  if (!is.null(arglist$project)) {
    if (!arglist$project) {
      proj <- "none"
    } else {
      proj <- arglist$project.args$projection
      arglist$project.args$projection <- NULL
      proj.args <- arglist$project.args
    }
  }
  if (!is.null(arglist$map.grid)) {
    warning("map.grid has been deprecated.  Consider specifying paxes.args")
    arglist$map.grid <- NULL
  }
  if (!is.null(arglist$map.poly)) {
    warning("map.poly has been deprecated.
            The argument has been renamed \"lines\".
            poly.args has been renamed \"lines.args\".
            Attempting to translate deprecated arguments.")
    arglist$lines <- arglist$map.poly
    arglist$lines.args <- arglist$poly.args
    arglist$map.poly <- NULL
    arglist$poly.args <- NULL
  }
  if (!is.null(arglist$map.points)) {
    warning("map.points has been deprecated.
            The argument has been renamed \"points\".
            Attempting to translate deprecated argument.")
    arglist$points <- arglist$map.points
    arglist$map.points <- NULL
  }
  if (length(lratio) != 1) {
    stop("lratio should be a single positive number")
  }
  
  if (!is.null(arglist$mratio)) {
    warning("mratio has been deprecated.  lratio should be used.
            lratio is the inverse of mratio.
            Attempting to translate deprecated argument.")
    lratio <- 1/arglist$mratio
    arglist$mratio <- NULL
  }
  if (!is.list(proj.args)) {
    stop("proj.args should be a list")
  }
  return(list(legend = legend, proj = proj, proj.args = proj.args, lratio = lratio, 
              arglist = arglist))
}

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

