#' Display axes for projected coordinates
#' 
#' \code{paxes} addes x and y axes to an existing plot for projected
#' coordinates.
#' 
#' The \code{\link[mapproj]{mapproject}} function is used for
#' projection.
#' 
#' \code{axis.args} should be a named list matching the arguments
#' of \code{\link[graphics]{axis}}.  The exception is that
#' \code{xat} and \code{yat} can be specified to induce
#' different spacing of the ticks on the x and y axes.  Thus,
#' the \code{at} argument is ignored and replaced by \code{xat} and
#' \code{yat}, as appropriate.
#' 
#' @inheritParams pimage
#' @param xlim A vector with the minimum and maximum value of the x
#'   coordinates.  Taken from \code{par("usr")} if not provided.
#' @param ylim A vector with the minimum and maximum value of the y
#'   coordinates.  Taken from \code{par("usr")} if not provided.
#' @param xaxp A vector of the form \code{c(x1, x2, n)} giving the 
#'   coordinates of the extreme tick marks and the number of intervals
#'   between tick marks.  Overrides \code{xlim}.
#' @param yaxp A vector of the form \code{c(x1, x2, n)} giving the 
#'   coordinates of the extreme tick marks and the number of intervals
#'   between tick marks.  Overrides \code{ylim}.
#' @param grid A logical value indicating whether grid lines should be
#'   displayed with the axes.  Default is \code{TRUE}.
#' @param axis.args A named list with components matching the
#'   arguments of \code{\link[graphics]{axis}}.  See Details and 
#'   Examples.
#' @param ... Other arguments passed to the \code{[graphics]{lines}} 
#'   function used to plot the grid lines.
#' @seealso \code{\link[graphics]{image}},
#'   \code{\link[mapproj]{mapproject}}
#' @return NULL
#' @examples
#' data(narccap)
#' # plot image using mercator projection (w/o axes)
#' pimage(lon, lat, tasmax[,,1], proj = "mercator", axes = FALSE)
#' # add axes with grey grid lines, blue text, and custom spacing
#' paxes("mercator", xlim = range(lon), ylim = range(lat), 
#'       col = "grey", 
#'       axis.args = list(col.axis = "blue", 
#'                        xat = c(-160, -100, -90, -80, -20)))
#' @export
paxes <- function(proj, xlim, ylim, xaxp, yaxp, grid = TRUE, axis.args, 
  ...) {
  if (missing(xlim)) 
    xlim <- par("usr")[1:2]
  if (missing(ylim)) 
    ylim <- par("usr")[3:4]
  if (missing(xaxp)) 
    xaxp <- NULL
  if (missing(yaxp)) 
    yaxp <- NULL
  if (missing(axis.args)) 
    axis.args <- list()
  arg.check.paxes(proj, xlim, ylim, xaxp, yaxp, grid)
  
  # create axis ticks, pre projection
  if (!is.null(xaxp)) {
    xat <- graphics::axTicks(1, axp = xaxp)
  } else {
    xat <- pretty(xlim)
  }
  if (!is.null(yaxp)) {
    yat <- graphics::axTicks(2, axp = yaxp)
  } else {
    yat <- pretty(ylim)
  }
  
  if (!is.null(axis.args$xat)) {
    xat <- axis.args$xat
    axis.args$xat <- NULL
  }
  if (!is.null(axis.args$yat)) {
    yat <- axis.args$yat
    axis.args$yat <- NULL
  }
  
  # function to call in do.call
  f <- graphics::axis
  if (proj == "none") {
    axis.args$side = 1
    axis.args$at <- xat
    do.call(f, axis.args)
    axis.args$side = 2
    axis.args$at <- yat
    do.call(f, axis.args)
    # graphics::axis(1, at = xat) graphics::axis(2, at = yat)
  } else {
    # convert axis coordinates add grid lines, if desired
    xe <- numeric(length(xat))
    for (i in seq_along(xat)) {
      xl <- cbind(xat[i], seq(min(yat), max(yat), len = 25))
      pxl <- mapproj::mapproject(xl[, 1], xl[, 2])
      if (grid) {
        graphics::lines(pxl$x, pxl$y, ...)
        # lines(pxl$x, pxl$y)
      }
      # need to interpolate to edge of plot
      xe[i] <- stats::spline(pxl$y, pxl$x, xout = par("usr")[3])$y
    }
    ye <- numeric(length(yat))
    for (j in seq_along(yat)) {
      yl <- cbind(seq(min(xat), max(xat), len = 25), yat[j])
      pyl <- mapproj::mapproject(yl[, 1], yl[, 2])
      if (grid) {
        graphics::lines(pyl$x, pyl$y, ...)
        # lines(pyl$x, pyl$y)
      }
      ye[j] <- stats::spline(pyl$x, pyl$y, xout = par("usr")[1])$y
    }
    axis.args$side = 1
    axis.args$at <- xe
    axis.args$labels <- xat
    do.call(f, axis.args)
    axis.args$side = 2
    axis.args$at <- ye
    axis.args$labels <- yat
    do.call(f, axis.args)
    # graphics::axis(1, at = xe, labels = xat) graphics::axis(2, at = ye,
    # labels = yat)
  }
  # add box to make things look normal
  box()
}

arg.check.paxes <- function(proj, xlim, ylim, xaxp, yaxp, grid) {
  if (length(proj) != 1) {
    stop("proj should be a single character string")
  }
  if (!is.character(proj)) {
    stop("proj should be a single character string")
  }
  if (proj != "none") {
    p <- try(mapproj::.Last.projection(), silent = TRUE)
    if (class(p) == "try-error") {
      stop("No projection has been used in current plot.
           Set \"proj\" in pimage or autoimage functions.")
    } else {
      if (p$projection != proj) {
        stop("proj and last projection used do not match")
      }
    }
  }
  if (length(xlim) != 2) {
    stop("xlim should be a numeric vector of length 2")
  }
  if (!is.numeric(xlim)) {
    stop("xlim should be a numeric vector of length 2")
  }
  if (length(ylim) != 2) {
    stop("ylim should be a numeric vector of length 2")
  }
  if (!is.numeric(ylim)) {
    stop("ylim should be a numeric vector of length 2")
  }
  if (!is.null(xaxp)) {
    if (length(xaxp) != 3) {
      stop("xaxp should be a vector of length 3.  See ?par")
    }
  }
  if (!is.null(yaxp)) {
    if (length(yaxp) != 3) {
      stop("yaxp should be a vector of length 3.  See ?par")
    }
  }
  if (length(grid) != 1) {
    stop("grid should be a logical value")
  }
  if (!is.logical(grid)) {
    stop("grid should be a logical value")
  }
}
