#' Projected segments function
#' 
#' \code{psegments} takes pairs of coordinates and draws line segments
#' between them, possibly after projection.
#' 
#' The \code{\link[mapproj]{mapproject}} function is used for 
#' projection.
#' 
#' @param x0,y0 coordinates of points from which to draw.
#' @param x1,y1 coordinates of points to which to draw
#' @inheritParams pimage
#' @param ... Additional arguments to pass to the 
#'   \code{\link[graphics]{segments}} function.
#' @seealso \code{\link[graphics]{segments}}, 
#'   \code{\link[mapproj]{mapproject}}, 
#'   \code{\link[autoimage]{pimage}}
#' @return NULL
#' @examples
#' data(narccap)
#' # plot image using bonne projection (w/o grid lines)
#' pimage(lon, lat, tasmax[,,1], proj = "bonne",
#'        parameters = 40, paxes.args = list(grid = FALSE))
#' # some locations for u.s. cities
#' # taken from data(us.cities, package = "maps")
#' boston <- c(-71.02, 42.34)
#' la <- c(-118.41, 34.11)
#' ny <- c(-73.94, 40.67)
#' sf <- c(-122.45, 37.77)
#' # plot segments between sf, la and ny boston
#' x0 <- c(sf[1], ny[1])
#' y0 <- c(sf[2], ny[2])
#' x1 <- c(la[1], boston[1])
#' y1 <- c(la[2], boston[2])
#' psegments(x0, y0, x1, y1, proj = "bonne", lwd = 3)
#' citycoords <- rbind(sf, la, ny, boston)
#' cityxy <- list(x = citycoords[,1], y = citycoords[,2])
#' citynames <- c("san francisco", "los angeles", "new york", "boston") 
#' ptext(cityxy, labels = citynames, proj = 'bonne')
#' @export
psegments <- function(x0, y0, x1 = x0, y1 = y0, proj, ...) {
  arglist <- list(...)
  object0 <- arg.check.plines(x0, y0, "b", proj, arglist)
  object1 <- arg.check.plines(x1, y1, "b", proj, arglist)
  object <- arglist
  object$x0 <- object0$x
  object$y0 <- object0$y
  object$x1 <- object1$x
  object$y1 <- object1$y
  if (proj != "none") {
    projxy0 <- mapproj::mapproject(object0$x, object0$y)
    projxy1 <- mapproj::mapproject(object1$x, object1$y)
    
    object$x0 <- projxy0$x
    object$y0 <- projxy0$y
    object$x1 <- projxy1$x
    object$y1 <- projxy1$y
  }
  f <- graphics::segments
  do.call(f, object)
}
