#' Projected polygon function
#' 
#' \code{ptext} draws polygons using the vertices given in \code{x} 
#' and \code{y}, possibly with projected coordinates.
#' 
#' The \code{\link[mapproj]{mapproject}} function is used for 
#' projection.
#' 
#' @inheritParams graphics::polygon
#' @inheritParams pimage
#' @seealso \code{\link[graphics]{polygon}}, 
#'   \code{\link[mapproj]{mapproject}}, 
#'   \code{\link[autoimage]{pimage}}
#' @return NULL
#' @examples
#' data(narccap)
#' # plot image using bonne projection (w/o grid lines)
#' pimage(lon, lat, tasmax[,,1], proj = "bonne",
#'        parameters = 40, paxes.args = list(col = "grey"))
#' # filled polygon for Colorado border
#' data(copoly)
#' ppolygon(copoly, proj = "bonne", col = "orange")
#' @export
ppolygon <- function(x, y = NULL, proj, ...) {
  arglist <- list(...)
  object <- arg.check.ppolygon(x, y, proj, arglist)
  if (proj != "none") {
    projxy <- mapproj::mapproject(object$x, object$y)
    object$x <- projxy$x
    object$y <- projxy$y
  }
  f <- graphics::polygon
  do.call(f, object)
}

arg.check.ppolygon <- function(x, y, proj, arglist) {
  if (is.list(x)) {
    if (is.null(x$x) | is.null(x$y)) {
      stop("If x is a list, it should have arguments x and y")
    }
    if (length(x$x) != length(x$y)) {
      stop("x$x and x$y should have the same length")
    }
    if (!is.numeric(x$x) | !is.numeric(x$y)) {
      stop("x and y should be numeric")
    }
    y <- x$y
    x <- x$x
  } else {
    if (is.null(y)) {
      y <- x
      x <- seq_len(x)
    } else {
      if (!is.numeric(x) | !is.numeric(y)) {
        stop("x and y should be numeric")
      }
      if (length(x) != length(y)) {
        stop("x and y should have the same length")
      }
    }
  }
  if (length(proj) != 1) {
    stop("proj should be a single character string")
  }
  if (!is.character(proj)) {
    stop("proj should be a single character string")
  }
  if (proj != "none") {
    p <- try(mapproj::.Last.projection(), silent = TRUE)
    if (class(p) == "try-error") {
      stop("no projection has been used in current plot, so no projection can be used with plines")
    } else {
      if (p$projection != proj) {
        stop("proj and last projection used do not match")
      }
    }
  }
  
  arglist$x <- x
  arglist$y <- y
  arglist
}
