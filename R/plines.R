#' Projected lines function
#' 
#' \code{plines} takes coordinates and joins the corresponding points
#' with line segments, possibly after projection.
#' 
#' The \code{\link[mapproj]{mapproject}} function is used for
#' projection.
#' 
#' @inheritParams graphics::lines
#' @inheritParams pimage
#' @seealso \code{\link[autoimage]{pimage}},
#'   \code{\link[mapproj]{mapproject}}, \code{\link[graphics]{lines}}
#' @return NULL
#' @examples
#' data(narccap)
#' # plot image using bonne projection (w/o grid lines)
#' pimage(lon, lat, tasmax[,,1], proj = "bonne",
#'        parameters = 40, paxes.args = list(grid = FALSE))
#' # get national boundaries
#' data(worldMapEnv, package = "maps")
#' worldpoly <- maps::map("world", plot = FALSE)
#' # add boundaries to existing plot
#' plines(worldpoly, proj = "bonne")
#' @export
plines <- function(x, y = NULL, type = "l", proj, ...) {
  arglist <- list(...)
  object <- arg.check.plines(x, y, type, proj, arglist)
  if (proj != "none") {
    projxy <- mapproj::mapproject(object$x, object$y)
    object$x <- projxy$x
    object$y <- projxy$y
  }
  f <- graphics::lines
  do.call(f, object)
}

arg.check.plines <- function(x, y, type, proj, arglist) {
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
  if (!is.element(type, c("b", "c", "h", "l", "n", "o", "p", "s"))) {
    stop("invalid type argument")
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
      stop("No projection has been used in current plot.
           Set \"proj\" in the pimage or autoimage functions.")
    } else {
      if (p$projection != proj) {
        stop("proj and last projection used do not match")
      }
    }
  }
  
  arglist$x <- x
  arglist$y <- y
  arglist$type <- type
  arglist
}
