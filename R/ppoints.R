#' Projected points function
#' 
#' \code{ppoints} draws a sequence of points for projected 
#' coordinates.
#' 
#' The \code{\link[mapproj]{mapproject}} function is used for 
#' projection.
#' 
#' @inheritParams graphics::points
#' @inheritParams pimage
#' @param ... Further graphical parameters passed to the 
#'   \code{\link[graphics]{points}} function.
#' @seealso \code{\link[graphics]{points}}, 
#'   \code{\link[mapproj]{mapproject}}, 
#'   \code{\link[autoimage]{pimage}}
#' @return NULL
#' @examples
#' data(narccap)
#' # plot image using bonne projection (w/o grid lines)
#' pimage(lon, lat, tasmax[,,1], proj = "bonne",
#'        parameters = 40, paxes.args = list(grid = FALSE))
#' # get U.S. cities with population of about 40k or more
#' data(us.cities, package = "maps")
#' # add cities to existing plot
#' ppoints(us.cities$long, us.cities$lat, proj = "bonne")
#' @export
ppoints <- function(x, y = NULL, type = "p", proj, ...) {
  arglist <- list(...)
  object <- arg.check.plines(x, y, type, proj, arglist)
  if (proj != "none") {
    projxy <- mapproj::mapproject(object$x, object$y)
    object$x <- projxy$x
    object$y <- projxy$y
  }
  
  f <- graphics::points
  do.call(f, object)
}
