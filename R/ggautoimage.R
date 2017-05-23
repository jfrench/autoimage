#' Display images using ggplot2
#' 
#' \code{ggautoimage} produces a sequence of images in a manner 
#' similar to \code{\link[autoimage]{autoimage}} using the 
#' \code{\link[ggplot2]{ggplot2}} package.
#' 
#' If \code{x} and \code{y} do not form a regular grid, then the 
#' \code{\link[MBA]{mba.surf}} function is used to
#' interpolate the locations onto a regular grid before constructing
#' the image.  This interpolation can be customized by passing
#' \code{interp.args} through \code{...}.  \code{interp.args} should
#' be a named list with components matching the non \code{xyz} 
#' arguments of the \code{\link[MBA]{mba.surf}} function.
#' 
#' When \code{proj != "none"}, the \code{\link[mapproj]{mapproject}} 
#' function is used to project the \code{x} and \code{y} coordinates. 
#' In that case, \code{proj} must correspond to one of the choices for
#' the \code{projection} argument in the 
#' \code{\link[mapproj]{mapproject}} function.  Necessary arguments
#' for \code{\link[mapproj]{mapproject}} should be provided through
#' the \code{parameters} and \code{orientation} arguments. 
#' See Examples or \code{\link[mapproj]{mapproject}} for more 
#' details.
#' 
#' Lines can be added to each image by providing the \code{lines}
#' argument.  In that case, \code{lines}
#' should be a list with components \code{x} and \code{y} 
#' specifying the locations to draw the lines.  If more than
#' one unconnected line should be drawn, then the coordinates
#' should be separated by NA.  e.g., to draw a line from (1, 1) to
#' (2, 2) and (3, 3) to (4, 4) (with a gap between the two lines), 
#' you would specify lines as 
#' lines(x = c(1:2, NA, 3:4), y =c(1:2, NA, 3:4)).  Also, see 
#' Examples.
#' 
#' Points can be added to each image by providing the \code{points}
#' argument.  In that case, \code{points}
#' should be a list with components \code{x} and \code{y} 
#' specifying the locations to draw the points.
#' 
#' @inheritParams pimage
#' @param x A numeric vector specifying the x coordinate locations.
#' @param y A numeric vector specifying the y coordinate locations.
#' @param z A numeric vector specifying the response for each (x,y)
#' location.  
#' @param f A factor variable distinguishing between different facets,
#' i.e., the different images to be constructed.
#' @param lines A named list with components \code{x} and \code{y}
#' specifiying the locations to be connected by lines.  Distinct
#' lines should be separated by \code{NA} values.  See Details.
#' @param points A named list with components \code{x} and \code{y}
#' specifiying the locations to be plot points.
#' @param interp.args A named list with component matching the 
#' non \code{xyz} arguments of the 
#' \code{\link[MBA]{mba.surf}} function.  Used to customize
#' interpolation, when required.
#' @seealso \code{\link[autoimage]{autoimage}},
#'   \code{\link[fields]{image.plot}}, \code{\link[graphics]{axis}}
#' @return NULL
#' @importFrom graphics axTicks axis box image layout par points lines
#'   mtext
#' @examples
#' data(narccap)
#' # setup image for two days of narccap data
#' x <- rep(c(lon), 2)
#' y <- rep(c(lat), 2)
#' z <- c(tasmax[, , 1:2])
#' f <- factor(rep(c("day 1", "day 2"), each = length(lon)))
#' # load national borders
#' data("worldMapEnv", package = "maps")
#' lines <- maps::map("world", plot = FALSE)
#' # obtain us captial cities
#' data(us.cities, package = "maps")
#' cap <- us.cities[us.cities$capital == 2, ]
#' # convert to list format
#' points <- list(x = cap$lon, y = cap$lat)
#' 
#' \dontrun{
#' # basic images
#' ggautoimage(x, y, z, f)
#' # basic images with national borders and U.S. captials
#' ggautoimage(x, y, z, f, lines = lines, points = points)
#' # project coordinates with national borders and U.S. capitals
#' ggautoimage(x, y, z, f, lines = lines, points = points,
#'             proj = "bonne", parameters = 40)
#' # finer interpolation grid
#' ggautoimage(x, y, z, f, lines = lines, points = points,
#'             interp.args = list(no.X = 100, no.Y = 100))
#' }
#' @export
ggautoimage <- function(x, y, z, f, proj = "none", parameters, orientation, 
  lines, points, interp.args) {
  if (missing(f)) 
    f <- factor(rep(1, length(x)))
  if (missing(parameters)) 
    parameters <- NULL
  if (missing(orientation)) 
    orientation <- NULL
  if (missing(interp.args)) 
    interp.args <- list()
  if (missing(lines)) 
    lines <- NULL
  if (missing(points)) 
    points <- NULL
  arg.check.ggautoimage(x, y, z, f, proj, lines, points, interp.args)
  df <- ggautoimage.xyz.setup(x, y, z, f, interp.args)
  p <- ggplot2::ggplot(df) + ggplot2::geom_tile(ggplot2::aes(x = x, y = y, 
    fill = z)) + ggplot2::facet_wrap(~f) + ggplot2::xlim(min(df$x), 
    max(df$x)) + ggplot2::ylim(min(df$y), max(df$y))
  
  if (!is.null(lines)) {
    linesdf <- ggautoimage.lines.setup(lines)
    p <- p + ggplot2::geom_path(ggplot2::aes(x = x, y = y), data = linesdf)
  }
  
  if (!is.null(points)) {
    pointsdf <- ggautoimage.points.setup(points)
    p <- p + ggplot2::geom_point(ggplot2::aes(x = x, y = y), data = pointsdf)
  }
  
  if (proj != "none") {
    p <- p + ggplot2::coord_map(project = proj, parameters = parameters, 
      orientation = orientation)
  }
  p
}

# setup lines for ggplot2 compatibility
ggautoimage.lines.setup <- function(x) {
  y <- x$y
  x <- x$x
  a <- rle(is.na(x))
  b <- rep(seq_along(a$lengths), times = a$lengths)
  ldf <- data.frame(x = x, y = y, g = b)
  ldf
}

# setup points for ggplot2 compatibility
ggautoimage.points.setup <- function(x) {
  y <- x$y
  x <- x$x
  pointsdf <- data.frame(x = x, y = y)
  stats::na.omit(pointsdf)
}

# argument checking
arg.check.ggautoimage <- function(x, y, z, f, proj, lines, points, interp.args) {
  if (!is.vector(x) | !is.vector(y) | !is.vector(z)) {
    stop("x, y, and z must be vectors")
  }
  if (length(x) != length(y) | length(x) != length(z)) {
    stop("x, y, and z must have the same length")
  }
  if (!is.numeric(x) | !is.numeric(y) | !is.numeric(z)) {
    stop("x, y, and z must be numeric")
  }
  if (length(x) != length(f)) {
    stop("f must have the same length as x, y, and z")
  }
  if (!is.factor(f)) {
    stop("f must be a factor")
  }
  if (length(proj) != 1) {
    stop("proj must be a single character string")
  }
  if (!is.character(proj)) {
    stop("proj must be a single character string")
  }
  if (!is.list(interp.args)) {
    stop("interp.args must be a list")
  }
  if (!is.null(lines)) {
    if (!is.list(lines)) {
      stop("lines must be a list")
    }
    if (is.null(lines$x) | is.null(lines$y)) {
      stop("lines must have components x and y")
    }
    if (length(lines$x) != length(lines$y)) {
      stop("lines$x and lines$y must have same length")
    }
    if (!is.numeric(lines$x) | !is.numeric(lines$y)) {
      stop("lines$x and lines$y must be numeric")
    }
  }
  if (!is.null(points)) {
    if (!is.list(points)) {
      stop("points must be a list")
    }
    if (is.null(points$x) | is.null(points$y)) {
      stop("points must have components x and y")
    }
    if (length(points$x) != length(points$y)) {
      stop("points$x and points$y must have same length")
    }
    if (!is.numeric(points$x) | !is.numeric(points$y)) {
      stop("points$x and points$y must be numeric")
    }
  }
  if (!is.list(interp.args)) {
    stop("interp.args must be a list")
  }
}

# setup x, y, z for ggplot2 compatibility
# f is the factor
# interp.args is the arguments for MBA::mba.surf
ggautoimage.xyz.setup <- function(x, y, z, f, interp.args) {
  df <- data.frame(x, y, z, f)
  split_df <- split(df, f = f)
  for (i in seq_along(split_df)) {
    xi <- split_df[[i]]$x
    yi <- split_df[[i]]$y
    zi <- split_df[[i]]$z
    fi <- split_df[[i]]$f[1]
    uxi <- unique(xi)
    uyi <- unique(yi)
    # if not on a regular grid, interpolate to regular grid
    if (length(uxi) * length(uyi) != length(zi)) {
      temp.interp.args <- interp.args
      temp.interp.args$xyz <- cbind(x, y, z)
      
      # convert from old format
      if (!is.null(temp.interp.args$xo)) {
        warning("MBA::mba.surf is now used for prediction on a grid instead of the akima::interp function.  Attempting to automatically translate arguments.  Results may slightly differ from previous versions of the package.")
        temp.interp.args$no.X <- length(temp.interp.args$xo)
        temp.interp.args$xo <- NULL
      }
      if (!is.null(temp.interp.args$yo)) {
        warning("MBA::mba.surf is now used for prediction on a grid instead of the akima::interp function.  Attempting to automatically translate arguments.  Results may slightly differ from previous versions of the package.")
        temp.interp.args$no.Y <- length(temp.interp.args$yo)
        temp.interp.args$yo <- NULL
      }
      if (!is.null(temp.interp.args$linear)) {
        warning("MBA::mba.surf is now used for prediction on a grid instead of the akima::interp function.  Attempting to automatically translate arguments.  Results may slightly differ from previous versions of the package.")
        temp.interp.args$linear <- NULL
      }
      if (!is.null(temp.interp.args$extrap)) {
        warning("MBA::mba.surf is now used for prediction on a grid instead of the akima::interp function.  Attempting to automatically translate arguments.  Results may slightly differ from previous versions of the package.")
        temp.interp.args$extend <- temp.interp.args$extrap
        temp.interp.args$extrap <- NULL
      }
      if (!is.null(temp.interp.args$nx)) {
        warning("MBA::mba.surf is now used for prediction on a grid instead of the akima::interp function.  Attempting to automatically translate arguments.  Results may slightly differ from previous versions of the package.")
        temp.interp.args$no.X <- temp.interp.args$nx
        temp.interp.args$nx <- NULL
      }
      if (!is.null(temp.interp.args$ny)) {
        warning("MBA::mba.surf is now used for prediction on a grid instead of the akima::interp function.  Attempting to automatically translate arguments.  Results may slightly differ from previous versions of the package.")
        temp.interp.args$no.Y <- temp.interp.args$ny
        temp.interp.args$ny <- NULL
      }
      
      if (is.null(temp.interp.args$no.X)) {
        temp.interp.args$no.X <- 40
      }
      if (is.null(temp.interp.args$no.Y)) {
        temp.interp.args$no.Y <- 40
      }
      interpf <- MBA::mba.surf
      ixyz <- do.call(interpf, temp.interp.args)
      iout <- ixyz$xyz.est
      # x <- ixyz$xyz.est$x
      # y <- ixyz$xyz.est$y
      # z <- ixyz$xyz.est$z
      # print(ixyz)
      # bug fix for old version of MBA package
      if (length(iout$x) != length(iout$y)) {
        if (length(iout$x) != nrow(iout$z)) {
          iout$z <- matrix(c(iout$z), 
                           nrow = ncol(iout$z), 
                           ncol = nrow(iout$z))    
        }
      }
      
      # fun <- MBA::mba.surf
      # # interpolation output
      # iout <- do.call(fun, temp.interp.args)
      xy <- expand.grid(iout$x, iout$y)
      split_df[[i]] <- data.frame(x = xy[, 1], y = xy[, 2], z = c(iout$z), 
                                  f = fi)
    }
  }
  return(do.call("rbind", split_df))
}

