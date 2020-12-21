#' Rotate coordinates
#' 
#' \code{rotate} rotates the coordinates by angle theta around a pivot
#' point.
#' 
#' @param coords A 2-column matrix with the coordinates to be rotated.
#' @param theta The angle (in radians) to rotate the coordinates.
#' @param pivot The pivot point around which the coordinates are 
#'   rotated. Default is c(0, 0), i.e., the origin.
#' @export
#' @examples 
#' # coordinates to rotate
#' coords <- matrix(rnorm(20), ncol = 2)
#' # rotate coordinates pi/6 radians around the original
#' rcoords <- rotate(coords, pi/6)
#' #compare original coordinates to rotated coordinates
#' par(mfrow = c(1, 2))
#' plot(coords)
#' plot(rcoords)
rotate <- function(coords, theta, pivot = c(0, 0)) {
  arg.check.rotate(coords, theta, pivot)
  # rotation matrix
  rmat <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), 
    byrow = TRUE, ncol = 2)
  # rotate coordinates around pivot
  t(pivot + rmat %*% (t(coords) - pivot))
}

arg.check.rotate <- function(coords, theta, pivot) {
  if (!is.matrix(coords)) {
    stop("coords must be a matrix")
  }
  if (ncol(coords) != 2) {
    stop("coords must have two columns")
  }
  if (!is.numeric(coords)) {
    stop("coords must have numeric values")
  }
  if (length(theta) != 1) {
    stop("theta must be a single numeric value")
  }
  if (!is.numeric(theta)) {
    stop("theta must be a single numeric value")
  }
  if (length(pivot) != 2) {
    stop("pivot must be a numeric vector of length 2")
  }
  if (!is.numeric(pivot)) {
    stop("pivot must be a numeric vector of length 2")
  }
}
