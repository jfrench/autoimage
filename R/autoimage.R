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
#' list with elements match \code{\link[graphics]{mtext}}.
#' 
#' Lines can be added to each image by passing the \code{lines}
#' argument through \code{...}.  In that case, \code{lines}
#' should be a list with components \code{x} and \code{y} 
#' specifying the locations to draw the lines.  The appearance
#' of the plotted lines can be customized by passing
#' a named list called \code{lines.args} through \code{...}.
#' The elements of \code{lines.args} should match the 
#' elements of \code{\link[graphics]{lines}}.  See Examples.
#' 
#' Points can be added to each image by passing the \code{points}
#' argument through \code{...}.  In that case, \code{points}
#' should be a list with components \code{x} and \code{y} 
#' specifying the locations to draw the points.  The appearance
#' of the plotted points can be customized by passing
#' a named list called \code{points.args} through \code{...}.
#' The elements of \code{points.args} should match the 
#' elements of \code{\link[graphics]{points}}.  See Examples.
#' 
#' #' The legend scale can be modified by passing \code{legend.axis.args}
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
#' # options
#' data(co, package = "gear")
#' autoimage(co$lon, co$lat, co[,c("Al", "Ca")], common.legend = FALSE, 
#'           map = "county", main = c("Aluminum", "Cadmium"),
#'           points = list(x = co$easting, y = co$northing),
#'           points.args = list(pch = 20, col = "white"))
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
  if (common.legend & legend != "none") 
    autolegend()
  
  # plot outer title, if necessary
  if (outer.args$outer) 
    do.call("mtext", outer.args$mtext.args)
  
  # restore previous par() settings
  on.exit(par(curpar))
}

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
TRUE
