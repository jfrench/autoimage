# Setup relevant arguments for plotting using the pimage function Check
# arguments Set various defaults Project if necessary Determine whether
# lines or points should be added
pimage.setup <- function(xyz, legend = "none", proj = "none", parameters = NULL, 
  orientation = NULL, lratio = 0.2, map = "none") {
  x <- xyz$x
  y <- xyz$y
  z <- xyz$z
  arglist <- xyz$arglist
  if (length(proj) != 1) {
    stop("proj should be a single character string")
  }
  if (!is.character(proj)) {
    stop("proj should be a single character string")
  }
  # match legend argument
  legend <- try(match.arg(legend, c("none", "horizontal", "vertical")), 
    silent = TRUE)
  if (length(legend) != 1) {
    stop("legend should be a single logical value")
  }
  if (class(legend) == "try-error") {
    stop("invalid legend argument.  
   legend should be \"none\", \"horizontal\", or \"vertical\".")
  }
  if (length(lratio) != 1) {
    stop("lratio should be a positive number")
  }
  if (!is.numeric(lratio)) {
    stop("lratio should be a positive number")
  }
  if (lratio <= 0) {
    stop("lratio should be a positive number")
  }
  
  # check colors
  if (is.null(arglist$col)) {
    if (is.null(arglist$breaks)) {
      arglist$col <- viridis::viridis(64)
    } else {
      nb <- length(arglist$breaks)
      arglist$col <- viridis::viridis(nb - 1)
    }
  }
  
  
  
  # setup arguments for legend.scale function
  legend.scale.args <- list()
  # if(legend != "none"){
  legend.scale.args$zlim <- arglist$zlim
  if (is.null(arglist$zlim)) {
    arglist$zlim <- range(z, na.rm = TRUE)
    legend.scale.args$zlim <- arglist$zlim
  }
  legend.scale.args$col <- arglist$col
  
  if (!is.null(arglist$breaks)) {
    legend.scale.args$breaks <- arglist$breaks
  }
  legend.scale.args$axis.args <- arglist$legend.axis.args
  # remove non-graphical argument from arglist
  arglist$legend.axis.args <- NULL
  
  legend.mar <- arglist$legend.mar
  # remove non-graphical argument from arglist
  arglist$legend.mar <- NULL
  if (is.null(legend.mar)) {
    legend.mar <- automar(legend)
  }
  
  # check if there are points to plot
  points <- arglist$points
  arglist$points <- NULL
  if (!is.null(points)) {
    if (!is.list(points)) {
      stop("points must be a list with vectors x and y")
    }
    if (is.null(points$x) | is.null(points$y)) {
      stop("points must be a list with vectors x and y")
    }
    if (length(points$x) != length(points$y)) {
      stop("The x and y vectors in points should have the same length")
    }
  }
  points.args <- arglist$points.args
  arglist$points.args <- NULL
  points.args$proj <- proj
  points.args$x <- points
  
  if (length(map) != 1) {
    stop("map should be a single character string")
  }
  if (!is.character(map)) {
    stop("map should be a single character string")
  }
  if (!is.element(map, c("none", "county", "france", "nz", "state", "usa", 
    "world", "world2", "italy", "lakes"))) {
    # future maps to add "china", "japan", "nzHires", "rivers",
    # "world2Hires", # "worldHires"
    stop("invalid map choice")
  } else {
    if (map == "county") {
      utils::data("countyMapEnv", package = "maps")
      arglist$lines <- maps::map("county", plot = FALSE)
    } else if (map == "france") {
      utils::data("franceMapEnv", package = "maps")
      arglist$lines <- maps::map("france", plot = FALSE)
    } else if (map == "nz") {
      utils::data("nzMapEnv", package = "maps")
      arglist$lines <- maps::map("nz", plot = FALSE)
    } else if (map == "state") {
      utils::data("stateMapEnv", package = "maps")
      arglist$lines <- maps::map("state", plot = FALSE)
    } else if (map == "usa") {
      utils::data("usaMapEnv", package = "maps")
      arglist$lines <- maps::map("usa", plot = FALSE)
    } else if (map == "world") {
      utils::data("worldMapEnv", package = "maps")
      arglist$lines <- maps::map("world", plot = FALSE)
    } else if (map == "world2") {
      utils::data("world2MapEnv", package = "maps")
      arglist$lines <- maps::map("world2", plot = FALSE)
    } else if (map == "italy") {
      utils::data("italyMapEnv", package = "maps")
      arglist$lines <- maps::map("italy", plot = FALSE)
    } else if (map == "lakes") {
      utils::data("lakesMapEnv", package = "maps")
      arglist$lines <- maps::map("lakes", plot = FALSE)
    }
  }
  
  # check if there are lines to plot
  lines <- arglist$lines
  arglist$lines <- NULL
  if (!is.null(lines)) {
    if (!is.list(lines)) {
      stop("lines must be a list with vectors x and y")
    }
    if (is.null(lines$x) | is.null(lines$y)) {
      stop("lines must be a list with vectors x and y")
    }
    if (length(lines$x) != length(lines$y)) {
      stop("The x and y vectors in lines should have he same length")
    }
  }
  lines.args <- arglist$lines.args
  arglist$lines.args <- NULL
  lines.args$proj <- proj
  lines.args$x <- lines
  
  # setup interpolation arguments
  interp.args <- arglist$interp.args
  # remove non-graphical arguments, if provided
  arglist$interp.args <- NULL
  if (is.null(interp.args)) {
    interp.args <- list()
  }
  
  # Check for non-gridded points.  Interpolate onto regular grid.
  if (!is.matrix(x)) {
    if (length(x) == length(z)) {
      interp.args$x <- x
      interp.args$y <- y
      interp.args$z <- z
      if (requireNamespace("akima", quietly = TRUE)) {
        interpf <- akima::interp
      } else {
        stop("User must manually install the akima package to enable this functionality due to licensing restrictions")
      }
      ixyz <- do.call(interpf, interp.args)
      x <- ixyz$x
      y <- ixyz$y
      z <- ixyz$z
    }
  }
  
  paxes.args <- arglist$paxes.args
  if (!is.null(arglist$paxes.args)) {
    arglist$paxes.args <- NULL
  }
  if (is.null(arglist$xlim)) {
    arglist$xlim <- range(x)
  }
  paxes.args$xlim <- arglist$xlim
  if (is.null(arglist$ylim)) {
    arglist$ylim <- range(y)
  }
  paxes.args$ylim <- arglist$ylim
  paxes.args$xaxp <- arglist$xaxp
  paxes.args$yaxp <- arglist$yaxp
  paxes.args$proj <- proj
  paxes.args$axis.args <- arglist$axis.args
  arglist$axis.args <- NULL
  
  if (is.null(arglist$axes)) {
    axes <- TRUE
  } else {
    axes <- arglist$axes
  }
  # will plot axes manually, if necessary
  arglist$axes <- FALSE
  
  if (proj != "none") {
    # if (!is.list(proj.args)) stop("proj.args should be a list")
    arglist$asp <- 1
    if (!is.matrix(x)) {
      x <- matrix(x, nrow = dim(z)[1], ncol = dim(z)[2])
    }
    if (!is.matrix(y)) {
      y <- matrix(y, nrow = dim(z)[1], ncol = dim(z)[2], byrow = TRUE)
    }
    xv <- c(x)
    yv <- c(y)
    
    which.in <- which(xv >= arglist$xlim[1] & xv <= arglist$xlim[2] & 
      yv >= arglist$ylim[1] & yv <= arglist$ylim[2])
    
    projectxy <- mapproj::mapproject(c(x), c(y), projection = proj, 
      parameters = parameters, orientation = orientation)
    x <- matrix(projectxy$x, nrow = nrow(x))
    y <- matrix(projectxy$y, nrow = nrow(y))
    arglist$xlim <- range(x[which.in])
    arglist$ylim <- range(y[which.in])
  }
  
  # is the grid a regular grid
  regular <- ifelse(length(x) != nrow(z), FALSE, TRUE)
  # decide plotting function accordingly
  plotf <- fields::poly.image
  if (regular) 
    plotf <- graphics::image
  
  arglist$x <- x
  arglist$y <- y
  arglist$z <- z
  
  object <- list(plotf = plotf, arglist = arglist, legend = legend, legend.scale.args = legend.scale.args, 
    legend.mar = legend.mar, proj = proj, points = points, points.args = points.args, 
    lines = lines, lines.args = lines.args, axes = axes, paxes.args = paxes.args)
  return(object)
}
TRUE
