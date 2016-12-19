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
#' @inheritParams pimage
#' @param common.legend A logical value indicating whether a common legend scale should be used for all images provided in the \code{z} array.  Default is \code{TRUE}.  If \code{FALSE}, a separate legend is used for each image.
#' @param size A vector of length two indicating the number of rows and columns that should be used for the series of image data in \code{z}.  Note that \code{prod(size)} must match the length of the third dimension of \code{z} (if it is an array), or \code{c(1, 1)} if \code{z} is a matrix.
#' @param outer.title A title related to all of the images that is plotted in the outer margin of the figure.
#' @param ... Additional arguments passed to the \code{\link[graphics]{image}} or \code{\link[fields]{poly.image}} functions.  e.g., \code{xlab}, \code{ylab}, \code{xlim}, \code{ylim}, \code{zlim}, etc.
#' @seealso \code{\link[autoimage]{pimage}}
#' @return NULL
#' @examples
#' # Example from image function documentation
#' x <- y <- seq(-4*pi, 4*pi, len = 27)
#' r <- sqrt(outer(x^2, y^2, "+"))
#' z <- cos(r^2)*exp(-r/6)
#' image(z, col  = gray((0:32)/32))
#' autoimage(z, col  = gray((0:32)/32), legend = FALSE)
#'
#' # now with legend
#' autoimage(z, col  = gray((0:32)/32))
#' autoimage(z, col  = gray((0:32)/32), horizontal = FALSE)
#'
#' # add some customization
#' autoimage(x, y, z, xlab = "x1", ylab = "y1", main = "Math is beautiful ...")
#'
#' # now do some examples with multiple images
#' z2 <- cos(r^2/2)*exp(-r/3)
#' z3 <- cos(r^2/2)*exp(-r/6)
#' z4 <- cos(r^2/3)*exp(-r/5)
#' zarray <- abind::abind(z, z2, z3, z4, along = 3)
#'
#' # multiple images with common scale, separate titles
#' par(mar = c(4.1, 4.1, 2.1, 2.1))
#' autoimage3(x, y, zarray, main = letters[1:4], lratio = 1/3, legend = "none")
#' # change the orientation of the scale
#' autoimage3(x, y, zarray, legend = "v", lratio = 0.25, main = letters[1:4])
#' 
#' # add overall title to plots
#' autoimage(x, y, zarray, main = letters[1:4], size = c(2, 2),
#'           mratio = 4, mmar = c(4.1, 4.1, 2.1, 2.1),
#'           horizontal = FALSE, 
#'           outer.title = "Interesting images with colored title",
#'           mtext.args = list(col = "blue"))
#' # multiple images with separate legends
#' autoimage(x, y, zarray, size = c(2, 2),
#'           mratio = 4, mmar = c(4.1, 4.1, 2.1, 2.1),
#'           horizontal = FALSE, common.legend = FALSE,
#'           outer.title = "Interesting images")
#' 
#' # do some examples with an irregular grid
#' # load data from fields package
#' data(narccap)
#' # restructure data for 2 images
#' tasmax2 = tasmax[,,1:2]
#' tasmax4 = tasmax[,,1:4]
#' 
#' # plot irregularly gridded images
#' autoimage4(lon, lat, tasmax2, col = fields::tim.colors(12), 
#' size = c(1, 2), common.legend = FALSE, map = "usa")
#' 
#' autoimage4(lon, lat, tasmax2, col = fields::tim.colors(12), 
#' size = c(1, 2))
#' 
#' autoimage4(lon, lat, tasmax2, col = fields::tim.colors(12), 
#' size = c(1, 2), legend = "none")
#' 
#' # Do the same plot, but with a projection.
#' # Notice that the axis scales seem off because of the projection
#' autoimage3(lon, lat, tasmax2, col = fields::tim.colors(12), size = c(1, 2),
#'           proj = "albers",
#'           proj.args = list(parameters = c(33, 45)))
#' # compare the axes for the projected coordinates to the correct references lines using map.grid.
#' autoimage(lon, lat, tasmax2, col = fields::tim.colors(12), size = c(1, 2),
#'           project = TRUE, map.grid = TRUE,
#'           project.args = list(projection = "albers", parameters = c(33, 45)),
#'           grid.args = list(col = "black", nx = 5, ny = 5))
#' # turn axes off
#' autoimage(lon, lat, tasmax2, col = fields::tim.colors(12), size = c(1, 2),
#'           project = TRUE, map.grid = TRUE, axes = FALSE, xlab = "", ylab = "",
#'           project.args = list(projection = "albers", parameters = c(33, 45)),
#'           grid.args = list(col = "black", nx = 5, ny = 5))
#'
#' # more images in a plot.  Need to change mratio
#' autoimage(lon, lat, tasmax4, col = fields::tim.colors(12), size = c(2, 2), 
#'           horizontal = FALSE, mratio = 4)
#' autoimage(lon, lat, tasmax4, col = fields::tim.colors(12), size = c(2, 2), mratio = 4)
#'
#' # add a nice polygon to the images
#' library(maps) # need to get world map
#' # get the polygon for the world from the maps package
#' worldpoly = map("world", plot = FALSE)
# project and plot two images, no axes,
# with polygon of national boundaries and X marking a location in Wyoming, 
# adjust the default points options
#' autoimage(lon, lat, tasmax2, size = c(1, 2), project = TRUE, 
#'           project.args = list(projection = "albers", parameters = c(33, 45)),
#'           map.poly = worldpoly, axes = FALSE,
#'           mmar = c(0.5, 0.5, 0.5, 0.5), legend.mar = c(2, 0.5, 0.5, 0.5), 
#'           map.points = list(x = -108.529, y = 43.33091), 
#'           points.args = list(pch = 4, cex = 2, lwd = 2),
#'           outer.title = "NARCCAP output")
#' @export
autoimage4 = function(x, y, z, legend = "horizontal", proj = "none", proj.args, 
                      lratio = 0.2, common.legend = TRUE, map = "none", size, outer.title, ...){
  # obtain elements of ...
  arglist = list(...)
  mtext.args <- arglist$mtext.args
  arglist$mtext.args <- NULL

  # attempt to match deprecated arguments
  if(missing(proj.args)) proj.args <- list()
  argmatch <- autoimage.match.old.args(legend, proj, proj.args, lratio,
                                       arglist)
  # setup x, y, z properly
  if(missing(x)) x <- NULL
  if(missing(y)) y <- NULL
  if(missing(z)) z <- NULL
  tx <- ifelse(is.null(x), "", deparse(substitute(x)))
  ty <- ifelse(is.null(y), "", deparse(substitute(y)))
  verbose <- FALSE
  # just added
  if(is.null(arglist$legend.mar) & legend != "none"){
    arglist$legend.mar <- set.legend.mar(legend)
  }
  verbose <- FALSE
  xyz.list <- autoimage.xyz.setup(x, y, z, tx, ty, arglist, verbose, common.legend)
  # additional argument checking
  ng <- length(xyz.list)
  if(missing(size)) size <- autosize(ng)
  if(missing(outer.title)) outer.title <- NULL
  arg.check.autoimage(common.legend, size, outer.title, ng)
  if(!is.null(arglist$zlim)){
    if(is.list(arglist$zlim)) common.legend <- FALSE
  }
  outer <- ifelse(!is.null(outer.title), TRUE, FALSE)
  
  # if(missing(mtext.args)) mtext.args = list()
  # if(!is.list(mtext.args)) stop("mtext.args should be a list")
  curpar <- par(no.readonly = TRUE)
  curmar <- par()$mar # current mar values
  autolayout(size, legend = legend, common.legend = common.legend,
             lratio = lratio, outer = outer, show = TRUE,
             reverse = FALSE)
  for(i in seq_along(xyz.list)){
    par(mar = curmar)
    arglisti <- xyz.list[[i]]
    arglisti$legend = "none"
    arglisti$proj = proj
    arglisti$proj.args = proj.args
    arglisti$map = map
    do.call("pimage", arglisti)
    if(!common.legend & legend != "none") {
      autolegend()
    }
  }
    
  deficit <- prod(size) - ng
  if(!common.legend & legend != "none") deficit <- 2*deficit
  for(i in seq_len(deficit)){ blank.plot() }
  if(common.legend & legend != "none") autolegend() 

  # plot outer title, if necessary
  if(!is.null(outer.title)){
    mtext.args <- list()
    mtext.args$text = outer.title
    mtext.args$outer = TRUE
    do.call(mtext, mtext.args)
  }
  # restore previous par() settings
  on.exit(par(curpar))
}

arg.check.autoimage <- function(common.legend, size, outer.title, ng){
  if(length(common.legend) != 1) stop("common.legend should be a logical value")
  if(!is.logical(common.legend)) stop("commen.legend should be a logical value")
  if(length(size) != 2) stop("size should be a vector of length 2")
  if(!is.numeric(size)) stop("size should be a numeric vector")
  if(prod(size) < ng) stop("size is not large enough to hold all plots")
  if(!is.null(outer.title)){
    if(length(outer.title) != 1) stop("outer.title should have length 1")
    if(!is.character(outer.title)) stop("outer.title should be a character string")
  }
}

# create blank plot
blank.plot <- function(){
  graphics::plot(1:2, 1:2, type = "n", axes = FALSE ,ann=FALSE)
}
