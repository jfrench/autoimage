#' Projected segments function
#'
#' \code{psegments} takes pairs of coordinates and draws line segments
#' between them, possibly after projection.  
#' 
#' The \code{\link[mapproj]{mapproject}} function is used for projection.
#'
#' @param x0,y0 coordinates of points from which to draw.
#' @param x1,y1 coordinates of points to which to draw
#' @inheritParams pimage
#' @param ... Additional arguments to pass to the 
#' \code{\link[graphics]{segments}} function.
#' @seealso \code{\link[graphics]{segments}}, \code{\link[mapproj]{mapproject}}, \code{\link[autoimage]{pimage}}
#' @return NULL
#' @examples
#' data(narccap)
#' # plot image using bonne projection (w/o grid lines)
#' pimage(lon, lat, tasmax[,,1], proj = "bonne",
#'        proj.args = list(parameters = 40), 
#'        paxes.args = list(grid = FALSE))
#' # load some data for larger U.S. cities
#' data(us.cities, package = "maps")
#' cityxy <- list(x = us.cities$long[1:5], y = us.cities$lat[1:5])
#' # plot segments between cities with thick line
#' psegments(cityxy$x[1:4], cityxy$y[1:4],
#'           cityxy$x[2:5], cityxy$y[2:5], 
#'           proj = "bonne", lwd = 3)
#' @export
psegments <- function(x0, y0, x1 = x0, y1 = y0, proj, ...){
  arglist <- list(...)
  object0 <- arg.check.plines(x0, y0, "b", proj, arglist)
  object1 <- arg.check.plines(x1, y1, "b", proj, arglist)
  object <- arglist
  object$x0 <- object0$x
  object$y0 <- object0$y
  object$x1 <- object1$x
  object$y1 <- object1$y
  if(proj != "none"){
    projxy0 <- mapproj::mapproject(object0$x, object0$y)
    projxy1 <- mapproj::mapproject(object1$x, object1$y)
    
    object$x0 <- projxy0$x
    object$y0 <- projxy0$y
    object$x1 <- projxy1$x
    object$y1 <- projxy1$y
  }
  do.call("segments", object)
}