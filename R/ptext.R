#' Projected text function
#' 
#' \code{ptext} draws the character strings given in \code{labels} at
#' the provided coordinates, possibly after projection.
#' 
#' The \code{\link[mapproj]{mapproject}} function is used for 
#' projection.
#' 
#' A non-character \code{labels} argument is automatically converted 
#' to a \code{character} vector using \code{link[base]{as.character}}.
#' 
#' @inheritParams graphics::text
#' @inheritParams pimage
#' @seealso \code{\link[graphics]{text}}, 
#'   \code{\link[mapproj]{mapproject}},
#'   \code{\link[autoimage]{pimage}}
#' @return NULL
#' @examples
#' data(narccap)
#' # plot image using bonne projection (w/o grid lines)
#' pimage(lon, lat, tasmax[,,1], proj = "bonne",
#'        parameters = 40, paxes.args = list(grid = FALSE))
#' # get national boundaries
#' data(worldMapEnv, package = "maps")
#' worldpoly <- maps::map("world", plot = FALSE)
#' plines(worldpoly, proj = "bonne")
#' # add U.S. city names to existing plot
#' data(us.cities, package = "maps")
#' citysmall <- head(us.cities)
#' ptext(x = citysmall$lon, y = citysmall$lat, 
#'       labels = citysmall$name, proj = "bonne")
#' @export
ptext <- function(x, y = NULL, labels, proj, ...) {
  labels <- as.character(labels)
  arglist <- list(...)
  object <- arg.check.ptext(x, y, labels, proj, arglist)
  if (proj != "none") {
    projxy <- mapproj::mapproject(object$x, object$y)
    object$x <- projxy$x
    object$y <- projxy$y
  }
  f <- graphics::text
  do.call(f, object)
}

arg.check.ptext <- function(x, y, labels, proj, arglist) {
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
  if (length(labels) != length(x)) {
    stop("labels length does not match length of x")
  }
  if (!is.character(labels)) {
    stop("labels must be a character vector")
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
   Specify \"proj\" in pimage or autoimage function.")
    } else {
      if (p$projection != proj) {
        stop("proj and last projection used do not match")
      }
    }
  }
  
  arglist$x <- x
  arglist$y <- y
  arglist$labels <- labels
  arglist
}
TRUE
