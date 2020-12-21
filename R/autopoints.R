#' Automatic facetting of multiple projected scatterplots
#'
#' \code{autopoints} plots a sequence of scatterplots (with possibly 
#' projected coordinates) while also automatically plotting a 
#' color scale matching the image colors to the values of \code{z}.  
#' Many options are available for customization. See the Examples 
#' below or execute \code{vignette("autopoints")} to better
#' understand the possibilities.
#' 
#' The \code{n} argument specifies the desired number of color
#' partitions, but may not be exact. This is used to create
#' "pretty" color scale tick labels using the
#' \code{\link[base]{pretty}} function.
#' If \code{zlim} or \code{breaks} are provided, then the 
#' \code{\link[base]{pretty}} function is not used to determine
#' the color scale tick lables, and the user may need to
#' manually specify \code{breaks} to get the color scale to
#' have "pretty" tick labels. If \code{col} is specified,
#' then \code{n} is set to \code{length(col)}, but the 
#' functions otherwise works the same (i.e., pretty tick
#' labels are not automatic if \code{zlim} or \code{breaks}
#' are specified.
#' 
#' The \code{\link[mapproj]{mapproject}} function is used to project 
#' the \code{x} and \code{y} coordinates when \code{proj != "none"}.
#'
#' If multiple scatterplots are to be plotted (i.e., if 
#' \code{z} is a matrix with more than 1 column), then the
#' \code{main} argument can be a vector with length matching 
#' \code{ncol(z)}, and each successive element of the vector will 
#' be used to add a title to each successive scatterplot.  
#' See the Examples.
#'
#' Additionally, if \code{common.legend = FALSE}, then separate 
#' z limits and breaks for the z-axis of each image can be provided as a list.  
#' Specifically, if \code{ncol(z) == k}, then \code{zlim} should 
#' be a list of length \code{k}, and each element of the list should 
#' be a 2-dimensional vector providing the lower and upper limit, 
#' respectively, of the legend for each image. Similarly,
#' the \code{breaks} argument should be a list of length \code{k},
#' and each element of the list should be a vector specifying
#' the breaks for the color scale for each plot. Note that
#' the length of each element of breaks should be 1 greater
#' then the number of colors in the color scale.
#' 
#' The range of \code{zlim} is cut into \eqn{n} partitions, 
#' where \code{n} is the length of \code{col}.
#'
#' It is generally desirable to increase \code{lratio} when
#' more images are plotted simultaneously.
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
#' The axes can be modified by passing \code{axis.args}
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
#' to \code{autpoints} through \code{...}.  This should be a numeric
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
#' @inheritParams heat_ppoints
#' @inheritParams autolayout
#' @inheritParams autoimage
#' @param ... Additional arguments passed to the 
#' \code{\link[graphics]{plot}}. e.g., \code{xlab}, \code{ylab}, 
#' \code{xlim}, \code{ylim}, \code{zlim}, etc.
#' @seealso \code{\link[autoimage]{autoimage}}, \code{\link[autoimage]{heat_ppoints}}
#' @return NULL
#' @export
#' @examples
#' data(co, package = "gear")
#' easting = co$easting
#' northing = co$northing
#' # heated scatterplot for Aluminum and Cadmium
#' autopoints(easting, northing, co[,c("Al", "Ca")],
#'            common.legend = FALSE, map = "state",
#'            main = c("Al", "Ca"), lratio = 0.2,
#'            legend.mar = c(0.3, 0.1, 0.1, 0.1))
#' 
#' # more complicated heat scatterplot for Aluminum and
#' # Cadmium used more advanced options
#' autopoints(co$lon, co$lat, co[,c("Al", "Ca")],
#'           common.legend = FALSE, 
#'           map = "county", main = c("Aluminum", "Cadmium"),
#'           proj = "bonne", parameters = 40,
#'           text = list(x = c(-104.98, -104.80), y = c(39.74, 38.85), 
#'                       labels = c("Denver", "Colorado Springs")),
#'           text.args = list(col = "blue"))
autopoints <- function(x, y, z, legend = "horizontal",
                       proj = "none", parameters,
                       orientation, common.legend = TRUE,
                       map = "none", size, lratio, 
                       outer.title, n = 5, ...) {
  # obtain elements of ...
  arglist <- list(...)
  mtext.args <- arglist$mtext.args
  legend <- match.arg(legend, c("none", "horizontal", "vertical"))

  # set default for missing arguments
  if (missing(outer.title)) outer.title <- NULL
  if (missing(parameters)) parameters <- NULL
  if (missing(orientation)) orientation <- NULL

  verbose <- FALSE  # some debugging stuff
  # setup x, y, z information
  xyz.list <- autopoints_xyz_setup(x, y, z, 
                                   tx = deparse(substitute(x)), 
                                   ty = deparse(substitute(y)), 
                                   arglist = arglist,
                                   verbose = verbose,
                                   common.legend = common.legend,
                                   legend = legend,
                                   n = n)
  ng <- length(xyz.list)  # number of grids
  # additional argument checking
  if (missing(size)) 
    size <- autosize(length(xyz.list))
  if (missing(lratio)) {
    if (legend == "horizontal") {
      lratio = 0.1 + 0.1 * size[1]
    } else {
      lratio = 0.1 + 0.1 * size[2]
    }
  }

  # check other argument, specify outer arguments
  outer.args <- arg.check.autoimage(common.legend, size, outer.title, ng, mtext.args)
  
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
    do.call("heat_ppoints", arglisti)
    if (!common.legend & legend != "none") {
      autolegend()
    }
  }
  
  # add blank plots, if necessary
  deficit <- prod(size) - ng
  if (!common.legend & legend != "none") 
    deficit <- 2 * deficit
  for (i in seq_len(deficit)) {
    blank.plot()
  }
  
  # add common legend, if necessary
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

# sorts out x, y, and z for autoimage function
autopoints_xyz_setup <- function(x, y, z, tx, ty, arglist,
                                 verbose, common.legend = FALSE,
                                 legend = "none", 
                                 n) {
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
      arglist$xlab <- tx
  }
  if (is.null(arglist$ylab)) {
      arglist$ylab <- ty
  }
  
  if (is.null(arglist$xlim)) {
    arglist$xlim <- range(x, na.rm = TRUE)
  }
  if (is.null(arglist$ylim)) {
    arglist$ylim <- range(y, na.rm = TRUE)
  }
  
  if (!is.null(arglist$col)) {
    n <- length(arglist$col)
  }
  if (length(n) != 1 | !is.numeric(n) | n <= 1) {
    stop("n should be a positive integer")
  }
  
  # check x, y, z
  if (!is.vector(x) | !is.numeric(x)) {
    stop("x must be a numeric vector")
  }
  if (!is.vector(y) | !is.numeric(y)) {
    stop("y must be a numeric vector")
  }
  if (length(x) != length(y)) {
    stop("x and y must have the same length")
  }

  if (!is.vector(z) & is.null(dim(z))) {
    stop("z must be a vector or matrix-like (!is.null(dim(z)))")
  }
  if (is.vector(z)) {
    if (length(x) != length(z)) {
      stop("length(z) must equal length(x) when z is a vector")
    }
  }  
  # convert z to a matrix
  if (is.vector(z)) {
    z <- matrix(z, ncol = 1)
  }
  # make sure z is a matrix
  if (is.data.frame(z)) {
    z <- as.matrix(z)
  }
  if (length(dim(z)) != 2) {
    stop("z can have only two-dimensions if matrix-like")
  }
  if (nrow(z) != length(x)) {
    stop("nrow(z) must equal length(x) when z is matrix-like")
  }

  # set plotting options for zlim, breaks
  arglist <- auto_zlim_breaks_setup(arglist, n, z, common.legend)

  # set legend margins
  if (is.null(arglist$legend.mar) & legend != "none") {
    arglist$legend.mar <- automar(legend)
  }
  
  
  # set main and zlim
  nz = ncol(z)
  main <- auto_main_setup(arglist_main = arglist$main, nz)
  zlim <- arglist$zlim
  arglist$zlim <- NULL
  breaks <- arglist$breaks
  arglist$breaks <- NULL

  # construct list component for each column of z
  # column of z
  xyz.list <- vector("list", nz)
  # replicate each set of information into list
  for (i in seq_along(xyz.list)) {
    arglist0 <- arglist
    arglist0$main <- main[i]
    arglist0$zlim <- zlim[[i]]
    arglist0$breaks <- breaks[[i]]
    arglist0$x <- x
    arglist0$y <- y
    arglist0$z <- z[, i]
    xyz.list[[i]] <- arglist0
  }
  return(invisible(xyz.list))
}
