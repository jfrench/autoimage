#' Display image for projected coordinates
#' 
#' \code{pimage} plots an image for (potentially) projected locations.
#' A color scale is automatically provided with the image. The 
#' function is essentially an extension of the 
#' \code{\link[graphics]{image}} function and the \code{x} and 
#' \code{y} locations can be irregularly-spaced locations, sequences 
#' of increasing values for locations on a regular grid, or matrices 
#' (with dimensions matching those of \code{z}) for locations on an 
#' irregular grid.  Functionality for automatic projection is 
#' provided.
#' 
#' If \code{x}, \code{y}, and \code{z} are numeric vectors of the same
#' length, then the \code{\link[MBA]{mba.surf}} function is used to 
#' predict the response on a regular grid using multilevel
#' B-splines before constructing 
#' the image.  This interpolation can be customized by passing 
#' \code{interp.args} through \code{...}.  \code{interp.args} should 
#' be a named list with component matching the non \code{xyz}
#' arguments of the \code{\link[MBA]{mba.surf}} function.
#' 
#' If \code{x} are \code{y} are vectors of increasing values and 
#' \code{nrow(z) == length(x)} and \code{ncol(z) == length(y)}, then 
#' an image on a regular grid is constructed.
#' 
#' If \code{x}, \code{y} and \code{z} are matrices with the same 
#' dimensions, then an image for irregularly gridded data is 
#' constructed.
#' 
#' When \code{proj != "none"}, the \code{\link[mapproj]{mapproject}} 
#' function is used to project the \code{x} and \code{y} coordinates. 
#' In that case, \code{proj} must correspond to one of the choices for
#' the \code{projection} argument in the 
#' \code{\link[mapproj]{mapproject}} function.  Necessary arguments 
#' for \code{\link[mapproj]{mapproject}} should be provided via the 
#' \code{parameters} and \code{orientation} arguments. See Examples 
#' and the \code{\link[mapproj]{mapproject}} function.
#' 
#' Valid options for \code{legend} are \code{"none"}, 
#' \code{"horizontal"}, and \code{"vertical"}.  If \code{legend = 
#' "none"}, then no color scale is provided.  If \code{legend = 
#' "horizontal"}, then a color scale is included under the image.  If 
#' \code{legend = "vertical"}, then a color scale is added to the 
#' right of the image.
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
#' The image axes can be modified by passing \code{axis.args} through
#' \code{...}.  The argument should be a named list corresponding to
#' the arguments of the \code{\link[graphics]{axis}} function.  The
#' exception to this is that arguments \code{xat} and \code{yat} can
#' be specified (instead of \code{at}) to specify the location of the
#' x and y ticks.  If \code{xat} or \code{yat} are specified, then
#' this overrides the \code{xaxt} and \code{yaxt} arguments,
#' respectively.  See the \code{\link[autoimage]{paxes}} function to
#' see how \code{axis.args} can be used.
#' 
#' The legend margin can be customized by passing \code{legend.mar} to
#' \code{pimage} through \code{...}.  This should be a numeric vector
#' indicating the margins of the legend, identical to how 
#' \code{par("mar")} is specified.
#' 
#' The various options of the labeling, axes, and legend are largely
#' independent.  e.g., passing \code{col.axis} through \code{...} 
#' will not affect the axis unless it is passed as part of the 
#' named list \code{axis.args}.  However, one can set the various
#' \code{par} options prior to plotting to simultaneously
#' affect the appearance of multiple aspects of the plot.  See 
#' Examples.  After plotting, \code{reset.par()} can be used to reset 
#' the graphics device options to their default values. 
#' 
#' @param x,y Locations of grid points at which the values in \code{z}
#'   are measured.  The values must be finite and non-missing.  These 
#'   arguments can be either vectors or matrices depending on the type
#'   of data to be displayed.  See Details.
#' @param z A numeric or logical vector or matrix containing the 
#'   values to be plotted (NAs are allowed).
#' @param legend A character string indicating where the color scale 
#'   should be placed.  The default is \code{"horizontal"}.  The other
#'   valid options are \code{"none"} and \code{"vertical"}.
#' @param proj A character string indicating what projection should be
#'   used for the included \code{x} and \code{y} coordinates.  The 
#'   default is \code{"none"}.  The other valid choices correspond to 
#'   the \code{"projection"} argument in the 
#'   \code{\link[mapproj]{mapproject}} function, which is used for the
#'   projection.
#' @param parameters A numeric vector specifying the values of the 
#'   \code{parameters} argument in the 
#'   \code{\link[mapproj]{mapproject}}.  This may be necessary when
#'   \code{proj != "none"}.
#' @param orientation A vector \code{c(latitude,longitude,rotation)} 
#'   which describes where the "North Pole" should be when computing 
#'   the projection.  See \code{\link[mapproj]{mapproject}} for more
#'   details.
#' @param lratio A numeric value indicating the ratio of the smaller 
#'   dimension of the legend scale to the width of the image.  Default
#'   is \code{lratio = 0.2}.
#' @param map The name of the map to draw on the image. Default is 
#'   \code{"none"}.  Other options include \code{"world"}, 
#'   \code{"usa"}, \code{"state"}, \code{"county"}, \code{"france"}, 
#'   \code{"nz"} (New Zealand), \code{"italy"}, \code{"lakes"}, and
#'   \code{"world2"}, all from the \code{maps} package.
#' @param ... Additional arguments passed to the 
#'   \code{\link[graphics]{image}} or \code{\link[fields]{poly.image}}
#'   functions.  e.g., \code{xlab}, \code{ylab}, \code{xlim}, 
#'   \code{ylim}, \code{zlim}, etc.  Additionally, arguments that can 
#'   be used to further customize the plot (like adding lines or 
#'   points), as described in Details and Examples.
#' @seealso \code{\link[graphics]{image}}, 
#'   \code{\link[fields]{image.plot}}, \code{\link[graphics]{axis}}
#' @return NULL
#' @importFrom graphics axTicks axis box image layout par points lines
#'   mtext
#' @examples
#' # image plot for data on an irregular grid
#' pimage(lon, lat, tasmax[,,1], legend = "h", map = "world")
#' # same plot but with projection and vertical legend
#' pimage(lon, lat, tasmax[,,1], legend = "v", map = "world", 
#'        proj = "bonne", parameters = 45)
#' # different projection
#' pimage(lon, lat, tasmax[,,1], proj = "albers",
#'        parameters = c(33, 45), map = "world")
#' 
#' reset.par() # reset graphics device
#' # image plot for non-gridded data
#' data(co, package = "gear")
#' pimage(co$longitude, co$latitude, co$Al)
#' 
#' # show observed locations on image,
#' # along with Colorado border, locations of Denver and Colorado 
#' # Springs
#' data(copoly)
#' copoints <- list(x = co$lon, y = co$lat)
#' pimage(co$longitude, co$latitude, co$Al, 
#'        lines = copoly, 
#'        lines.args = list(lwd = 2, col = "grey"),
#'        points = copoints, 
#'        points.args = list(pch = 21, bg = "white"),
#'        text = list(x = c(-104.98, -104.80), y = c(39.74, 38.85), 
#'                    labels = c("Denver", "Colorado Springs")), 
#'        text.args = list(col = "purple"),
#'        xlim = c(-109.1, -102),
#'        ylim = c(36.8, 41.1))
#' 
#' # image plot for data on irregular grid
#' # notice the poor axis labeling
#' data(narccap)
#' pimage(lon, lat, tasmax[,,1], proj = "bonne",
#'        parameters = 45, map = "world")
#' # same plot but customize axis labeling 
#' # need to extend horizontally-running axis lines
#' # farther to the west and east
#' # also need the vertically-running lines
#' # to run further north/sount
#' # will need manual adjusting depending on size
#' # of current device 
#' pimage(lon, lat, tasmax[,,1], proj = "bonne",
#'        parameters = 45, map = "world", 
#'        xaxp = c(-200, 0, 10), yaxp = c(-10, 80, 9))
#' 
#' # the same effect can be acheived by specifying axis.args
#' # we also modify the color and size of the axis labels
#' pimage(lon, lat, tasmax[,,1], proj = "bonne",
#'        parameters = 45, map = "world", 
#'        axis.args = list(xat = seq(-200, 0, by = 20),
#'                         yat = seq(0, 70, by = 10),
#'                         col.axis = "blue", 
#'                         cex.axis = 0.5))
#' 
#' # modify colors of legend, map, line type for grid lines
#' # and customize axis
#' pimage(lon, lat, tasmax[,,1], 
#'        legend = "v", proj = "bonne", parameters = 45,
#'        map = "state",
#'        paxes.args = list(lty = 3),
#'        legend.axis.args = list(col = "blue", col.axis = "blue"),
#'        col = fields::tim.colors(64),
#'        xlab = "longitude",
#'        ylab = "latitude",
#'        main = "temperature (K)")
#' reset.par() # reset graphics device
#' 
#' # change many aspects of plot appearance using par
#' par(cex.axis = 0.5, cex.lab = 0.5, mgp = c(1.5, 0.5, 0),
#'     mar = c(2.1, 2.1, 4.1, 0.2), col.axis = "orange",
#'     col.main = "blue", family = "mono")
#' pimage(lon, lat, tasmax[,,1])
#' title("very customized plot")
#' reset.par()
#' 
#' @export
pimage <- function(x, y, z, legend = "horizontal", proj = "none", parameters, 
  orientation, lratio = 0.2, map = "none", ...) {
  # determine current par values to restore later
  if (missing(x)) 
    x <- NULL
  if (missing(y)) 
    y <- NULL
  if (missing(z)) 
    z <- NULL
  if (missing(parameters)) 
    parameters <- NULL
  if (missing(orientation)) 
    orientation <- NULL
  # obtain elements of ...
  arglist <- list(...)
  
  # setup x, y, z for plotting
  xyz <- pimage.xyz.setup(x = x, y = y, z = z,
                          tx = deparse(substitute(x)),
                          ty = deparse(substitute(y)),
                          arglist = arglist)
  
  # check/setup arguments for pimage
  object <- pimage.setup(xyz, legend, proj, parameters,
                         orientation, lratio, map)
  if (legend != "none") {
    .legend.mar(object$legend.mar)
  }
  .legend.scale.args(object$legend.scale.args)
  
  if (legend == "none") {
    do.call(object$plotf, object$arglist)
  } else {
    autolayout(size = c(1, 1), legend = legend, lratio = lratio, show = FALSE, 
      reverse = TRUE)
    autolegend()
    do.call(object$plotf, object$arglist)
  }
  
  # plot axes, lines, points if desired
  if (object$axes) {
    do.call("paxes", object$paxes.args)
  }
  if (!is.null(object$lines.args$x)) {
    do.call("plines", object$lines.args)
  }
  if (!is.null(object$points.args$x)) {
    f <- autoimage::ppoints
    do.call(f, object$points.args)
  }
  if (!is.null(object$text.args$x)) {
    do.call("ptext", object$text.args)
  }
  return(invisible(structure(object, class = "pimage")))
}

# Setup relevant arguments for plotting using the pimage function 
# Check arguments 
# Set various defaults 
# Project if necessary 
# Determine whether lines or points should be added
pimage.setup <- function(xyz, legend = "none", proj = "none", parameters = NULL, 
                         orientation = NULL, lratio = 0.2, map = "none") {
  x <- xyz$x
  y <- xyz$y
  z <- xyz$z
  arglist <- xyz$arglist
  if (length(proj) != 1) {
    stop("proj should be a single character string")
  }
  if (!is.character(proj)) {
    stop("proj should be a single character string")
  }
  # match legend argument
  legend <- try(match.arg(legend, c("none", "horizontal", "vertical")), 
                silent = TRUE)
  if (length(legend) != 1) {
    stop("legend should be a single logical value")
  }
  if (class(legend) == "try-error") {
    stop("invalid legend argument.  
         legend should be \"none\", \"horizontal\", or \"vertical\".")
  }
  if (length(lratio) != 1) {
    stop("lratio should be a positive number")
  }
  if (!is.numeric(lratio)) {
    stop("lratio should be a positive number")
  }
  if (lratio <= 0) {
    stop("lratio should be a positive number")
  }

  # setup x, y, and zlim in arglist
  arglist <- xyzlim_arglist_setup(x, y, z, arglist)
  
  # setup col argument for pimage
  arglist <- pimage_col_arglist_setup(arglist)

  # setup legend.scale.args from arglist
  legend.scale.args <- legend_scale_args_setup(arglist)

  legend.mar <- arglist$legend.mar
  # remove non-graphical argument from arglist
  arglist$legend.mar <- NULL
  if (is.null(legend.mar)) {
    legend.mar <- automar(legend)
  }

  # setup map information, if necessary
  if (map != "none") arglist$lines <- map_setup(map)
  
  # setup lines information, if necessary
  lines.args <- lines_args_setup(arglist, proj)
  
  # setup points information, if necessary
  points.args <- points_args_setup(arglist, proj)
  
  # setup text information, if necessary
  text.args = text_args_setup(arglist, proj)
  
  # Check for non-gridded points.  
  # Surface approximation onto regular grid.
  if (!is.matrix(x) & length(x) == length(z)) {
    xyz = interp_setup(x, y, z, arglist)
    x = xyz$x
    y = xyz$y
    z = xyz$z
  }
  
  # setup paxes information
  paxes.args <- paxes_args_setup(arglist, proj)
  
  # setup axes information
  axes <- axes_setup(arglist)
  
  if (proj != "none") {
    # if (!is.list(proj.args)) stop("proj.args should be a list")
    arglist$asp <- 1
    if (!is.matrix(x)) {
      x <- matrix(x, nrow = dim(z)[1], ncol = dim(z)[2])
    }
    if (!is.matrix(y)) {
      y <- matrix(y, nrow = dim(z)[1], ncol = dim(z)[2], byrow = TRUE)
    }
    xv <- c(x)
    yv <- c(y)
    
    which.in <- which(xv >= arglist$xlim[1] & xv <= arglist$xlim[2] & 
                      yv >= arglist$ylim[1] & yv <= arglist$ylim[2])
    
    projectxy <- mapproj::mapproject(c(x), c(y), projection = proj, 
                                     parameters = parameters, orientation = orientation)
    if (projectxy$error == 1) {
      warning("projection failed for some data")
    }
    x <- matrix(projectxy$x, nrow = nrow(x))
    y <- matrix(projectxy$y, nrow = nrow(y))
    
    # arglist$xlim <- range(x[which.in], na.rm = TRUE)
    # arglist$ylim <- range(y[which.in], na.rm = TRUE)
    
    # project limits
    sx = seq(arglist$xlim[1], arglist$xlim[2], len = 100)
    sy = seq(arglist$ylim[1], arglist$ylim[2], len = 100)
    # need to create grid of possibilities since grids are weird
    sg = expand.grid(sx, sy)
    # project grid
    project_lim <- mapproj::mapproject(sg[,1], sg[,2],
                                       projection = proj, 
                                       parameters = parameters,
                                       orientation = orientation)
    # take limits
    arglist$xlim <- range(project_lim$x, na.rm = TRUE)
    arglist$ylim <- range(project_lim$y, na.rm = TRUE)
  }
  
  # is the grid a regular grid
  regular <- ifelse(length(x) != nrow(z), FALSE, TRUE)
  # decide plotting function accordingly
  plotf <- fields::poly.image
  if (regular) 
    plotf <- graphics::image
  
  # clear unnecessary additional arguments
  arglist <- arglist_clean(arglist, image = TRUE)
  
  arglist$x <- x
  arglist$y <- y
  arglist$z <- z
  
  object <- list(plotf = plotf, arglist = arglist, legend = legend, 
                 legend.scale.args = legend.scale.args, 
                 legend.mar = legend.mar, proj = proj, 
                 # points = points,
                 points.args = points.args, 
                 # lines = lines,
                 lines.args = lines.args, 
                 axes = axes, paxes.args = paxes.args,
                 # text = text,
                 text.args = text.args)
  return(object)
}


#' setup x, y, and z for pimage function
#'
#' @param x vector or matrix of x coordinates
#' @param y vector or matrix of y coordinates
#' @param z vector or matrix of z values
#' @param tx text associated with x
#' @param ty text associated with y
#' @param arglist argument list
#' @noRd
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
  # if (!is.matrix(z) | !is.data.frame(z)) {
  if (is.null(dim(z))) {
    if (is.null(x) | is.null(y)) {
      stop("x and y must be specified when z is not a matrix")
    }
    if (!(length(x) == length(y))) {
      stop("x and y do not have the same length and/or dimensions")
    }
  } else {
    if (length(dim(z)) != 2) {
      stop("If z is a matrix-like object, (i.e., dim(z) != NULL), then it must be two-dimensional")
    }
    z <- as.matrix(z) # convert z to a matrix, just in case
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

