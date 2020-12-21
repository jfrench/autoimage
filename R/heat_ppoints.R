#' Display colored scatterplot for projected coordinates
#'
#' \code{heat_ppoints} plots a "heated" scatterplot for
#' (potentially) projected locations. A color scale is
#' automatically provided with the scatterplot. The function
#' is similar in purpose to \code{\link[autoimage]{pimage}},
#' but the \code{z}-values are not interpolated. The color
#' scale can be changed by passing a vector of colors to
#' the \code{col} argument.
#'
#' When \code{proj != "none"}, the
#' \code{\link[mapproj]{mapproject}} function is used to
#' project the \code{x} and \code{y} coordinates. In that
#' case, \code{proj} must correspond to one of the choices
#' for the \code{projection} argument in the
#' \code{\link[mapproj]{mapproject}} function.  Necessary
#' arguments for \code{\link[mapproj]{mapproject}} should be
#' provided via the \code{parameters} and \code{orientation}
#' arguments. See Examples and the
#' \code{\link[mapproj]{mapproject}} function.
#'
#' Valid options for \code{legend} are \code{"none"},
#' \code{"horizontal"}, and \code{"vertical"}.  If
#' \code{legend = "none"}, then no color scale is provided.
#' If \code{legend = "horizontal"}, then a color scale is
#' included under the plot.  If \code{legend = "vertical"},
#' then a color scale is added to the right of the plot.
#'
#' Lines can be added to each plot by passing the
#' \code{lines} argument through \code{...}.  In that case,
#' \code{lines} should be a list with components \code{x}
#' and \code{y} specifying the locations to draw the lines.
#' The appearance of the plotted lines can be customized by
#' passing a named list called \code{lines.args} through
#' \code{...}. The components of \code{lines.args} should
#' match the arguments of \code{\link[graphics]{lines}}.
#' See Examples.
#'
#' Points can be added to each image by passing the
#' \code{points} argument through \code{...}.  In that case,
#' \code{points} should be a list with components \code{x}
#' and \code{y} specifying the locations to draw the points.
#' The appearance of the plotted points can be customized by
#' passing a named list called \code{points.args} through
#' \code{...}. The components of \code{points.args} should
#' match the components of \code{\link[graphics]{points}}.
#' See Examples.
#'
#' Text can be added to each image by passing the
#' \code{text} argument through \code{...}.  In that case,
#' \code{text} should be a list with components \code{x} and
#' \code{y} specifying the locations to draw the text, and
#' \code{labels}, a component specifying the actual text to
#' write.  The appearance of the plotted text can be
#' customized by passing a named list called
#' \code{text.args} through \code{...}. The components of
#' \code{text.args} should match the components of
#' \code{\link[graphics]{text}}.  See Examples.
#'
#' The legend scale can be modified by passing
#' \code{legend.axis.args} through \code{...}.  The argument
#' should be a named list corresponding to the arguments of
#' the \code{\link[graphics]{axis}} function.  See Examples.
#'
#' The plot axes can be modified by passing
#' \code{axis.args} through \code{...}.  The argument should
#' be a named list corresponding to the arguments of the
#' \code{\link[graphics]{axis}} function.  The exception to
#' this is that arguments \code{xat} and \code{yat} can be
#' specified (instead of \code{at}) to specify the location
#' of the x and y ticks.  If \code{xat} or \code{yat} are
#' specified, then this overrides the \code{xaxt} and
#' \code{yaxt} arguments, respectively.  See the
#' \code{\link[autoimage]{paxes}} function to see how
#' \code{axis.args} can be used.
#'
#' The legend margin can be customized by passing
#' \code{legend.mar} to \code{heat_ppoints} through \code{...}.
#' This should be a numeric vector indicating the margins of
#' the legend, identical to how \code{par("mar")} is
#' specified.
#'
#' The various options of the labeling, axes, and legend are
#' largely independent.  e.g., passing \code{col.axis}
#' through \code{...} will not affect the axis unless it is
#' passed as part of the named list \code{axis.args}.
#' However, one can set the various \code{par} options prior
#' to plotting to simultaneously affect the appearance of
#' multiple aspects of the plot.  See Examples.  After
#' plotting, \code{reset.par()} can be used to reset the
#' graphics device options to their default values.
#'
#' @param x,y Numeric vectors of coordinates at which the
#'   values in \code{z} are measured.
#' @param z A numeric vector containing the values to be
#'   plotted.
#' @param legend A character string indicating where the
#'   color scale should be placed.  The default is
#'   \code{"horizontal"}.  The other valid options are
#'   \code{"none"} and \code{"vertical"}.
#' @param proj A character string indicating what projection
#'   should be used for the included \code{x} and \code{y}
#'   coordinates.  The default is \code{"none"}.  The other
#'   valid choices correspond to the \code{"projection"}
#'   argument in the \code{\link[mapproj]{mapproject}}
#'   function, which is used for the projection.
#' @param parameters A numeric vector specifying the values
#'   of the \code{parameters} argument in the
#'   \code{\link[mapproj]{mapproject}}.  This may be
#'   necessary when \code{proj != "none"}.
#' @param orientation A vector
#'   \code{c(latitude,longitude,rotation)} which describes
#'   where the "North Pole" should be when computing the
#'   projection.  See \code{\link[mapproj]{mapproject}} for
#'   more details.
#' @param lratio A numeric value indicating the ratio of the
#'   smaller dimension of the legend scale to the width of
#'   the image.  Default is \code{lratio = 0.2}.
#' @param map The name of the map to draw on the image.
#'   Default is \code{"none"}.  Other options include
#'   \code{"world"}, \code{"usa"}, \code{"state"},
#'   \code{"county"}, \code{"france"}, \code{"nz"} (New
#'   Zealand), \code{"italy"}, \code{"lakes"}, and
#'   \code{"world2"}, all from the \code{maps} package.
#' @param ... Additional arguments passed to the
#'   \code{\link[graphics]{plot}} function.  e.g.,
#'   \code{xlab}, \code{ylab}, \code{xlim}, \code{ylim},
#'   \code{zlim}, etc.  Additionally, arguments that can be
#'   used to further customize the plot (like adding lines
#'   or points), as described in Details and Examples.
#' @inheritParams base::pretty   
#' @seealso \code{\link[graphics]{plot}},
#'   \code{\link[graphics]{axis}},
#'   \code{\link[autoimage]{pimage}}
#' @return NULL
#' @export
#' @examples
#' data(co, package = "gear")
#' # heated scatterplot for data on an irregular grid
#' heat_ppoints(co$lon, co$lat, co$Al, legend = "v", map = "state")
#' reset.par()
#' 
#' # change color scale
#' heat_ppoints(co$lon, co$lat, co$Al, col = cm.colors(5))
#' reset.par()
#'
#' # Use custom border, x and y limits, breaks for legend axis
#' data(copoly)
#' heat_ppoints(co$lon, co$lat, co$Al, legend = "h",
#'        xlab = "longitude", ylab = "latitude",
#'        proj = "bonne", parameters = 40,
#'        lines = copoly,
#'        lines.args = list(lwd = 2, col = "grey"),
#'        xlim = c(-109.1, -102),
#'        ylim = c(36.8, 41.1),
#'        breaks = seq(0, 10, len = 6))
#' reset.par()
heat_ppoints <- function(x, y, z, legend = "horizontal",
                         proj = "none", parameters,
                         orientation, lratio = 0.2,
                         map = "none", n = 5, ...) {
  if (missing(parameters)) parameters <- NULL
  if (missing(orientation)) orientation <- NULL
  # obtain elements of ...
  arglist <- list(...)
  
  # setup x, y, z and arglist for plotting
  xyz <- heat_ppoints_xyz_setup(x = x, y = y, z = z,
                               tx = deparse(substitute(x)),
                               ty = deparse(substitute(y)),
                               arglist = arglist)
  # check/setup arguments for heat_ppoints
  object <- heat_ppoints_setup(xyz, legend, proj, parameters,
                               orientation, lratio, map, n = n)
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
  
  # if pch is between 19 and 25, we can color the middle and border separately
  if (!is.null(arglist$pch) & !is.null(arglist$border_col)) {
    if (arglist$pch >= 19) {
      arglist$bg = arglist$col
      arglist$col = arglist$border_col
    }
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
  return(invisible(structure(object, class = "heat_ppoints")))
}

# Setup relevant arguments for plotting using the heat_ppoints function 
# Check arguments 
# Set various defaults 
# Project if necessary 
# Determine whether lines or points should be added
heat_ppoints_setup <- function(xyz, legend = "none",
                               proj = "none",
                               parameters = NULL, 
                               orientation = NULL,
                               lratio = 0.2, map = "none",
                               n) {
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
    stop("legend should be a single character string")
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
  
  if (is.null(arglist$pch)) {
    arglist$pch <- 16
  }
  if (!is.null(arglist$col)) {
    n <- length(arglist$col)
  }
  if (length(n) != 1 | !is.numeric(n) | n <= 1) {
    stop("n should be a positive integer")
  }
  
  # set zlim and breaks for plotting
  zlim_breaks <- zlim_breaks_setup(arglist$zlim,
                                  arglist$breaks, n,
                                  range(z, na.rm = TRUE),
                                  arglist$col)
  arglist$zlim <- zlim_breaks$zlim
  arglist$breaks <- zlim_breaks$breaks
  if (is.null(arglist$col)) {
    arglist$col <- colorspace::sequential_hcl(n = length(arglist$breaks) - 1, palette = "Viridis")
  }
  
  legend.scale.args <- list()
  legend.scale.args$zlim <- arglist$zlim
  legend.scale.args$breaks <- arglist$breaks
  legend.scale.args$col <- arglist$col
  legend.scale.args$axis.args <- arglist$legend.axis.args
  # remove non-graphical argument from arglist

  # update color for points
  hpcol = as.character(cut(z, breaks = arglist$breaks, labels = arglist$col))
  arglist$col = hpcol
  
  if (!is.null(arglist$pch) & !is.null(arglist$border_col)) {
    if (arglist$pch >= 19) {
      arglist$bg = arglist$col
      arglist$col = arglist$border_col
    }
  }
  
  legend.mar <- arglist$legend.mar
  # remove non-graphical argument from arglist
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

  # setup paxes information
  paxes.args <- paxes_args_setup(arglist, proj)
  
  # setup axes information
  axes <- axes_setup(arglist)
  
  if (proj != "none") {
    arglist$asp <- 1
    which.in <- which(x >= arglist$xlim[1] & x <= arglist$xlim[2] & 
                      y >= arglist$ylim[1] & y <= arglist$ylim[2])
    
    projectxy <- mapproj::mapproject(x, y, projection = proj, 
                                     parameters = parameters,
                                     orientation = orientation)
    x <- projectxy$x
    y <- projectxy$y
    
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
  
  # store x and y for plotting
  arglist$x <- x
  arglist$y <- y
  
  # decide plotting function accordingly
  plotf <- graphics::plot

  # clear unnecessary additional arguments
  arglist <- arglist_clean(arglist, image = FALSE)

  object <- list(plotf = plotf, arglist = arglist,
                 legend = legend, 
                 legend.scale.args = legend.scale.args, 
                 legend.mar = legend.mar, proj = proj, 
                 points.args = points.args, 
                 lines.args = lines.args,
                 text.args = text.args,
                 paxes.args = paxes.args,
                 axes = axes)
  return(object)
}

#' Setup xyz and arglist for heat_ppoints
#'
#' @param x A numeric vector of x coordinates
#' @param y A numeric vector of y coordinates
#' @param z A numeric vector of response values
#' @param tx potential x-axis label
#' @param ty potential y-axis label
#' @param arglist A list of argument passed to heat_ppoints
#' @noRd
heat_ppoints_xyz_setup <- function(x, y, z, tx, ty, arglist) {
  # sort out x, y, z, labels, etc.  This is a revision of the beginning
  # of graphics::image
  if (is.null(arglist$xlab)) arglist$xlab <- tx
  if (is.null(arglist$ylab)) arglist$ylab <- ty
  if (!is.vector(x) | !is.numeric(x)) {
    stop("x must be a numeric vector")
  }
  if (!is.vector(y) | !is.numeric(y)) {
    stop("y must be a numeric vector")
  }
  if (!is.vector(z) | !is.numeric(z)) {
    stop("z must be a numeric vector")
  }
  if (length(x) != length(y)) stop("length(x) != length(y)")
  if (length(x) != length(z)) stop("length(x) != length(z)")

  # ensure x and y limits are provided
  if (is.null(arglist$xlim)) {
    arglist$xlim <- range(x, na.rm = TRUE)
  }
  if (is.null(arglist$ylim)) {
    arglist$ylim <- range(y, na.rm = TRUE)
  }
  return(list(x = x, y = y, z = z, arglist = arglist))
}
