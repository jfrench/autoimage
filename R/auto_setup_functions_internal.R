#' Setup main argument for autoimage, autopoints, etc.
#'
#' @param arglist_main The main argument of arglist (i.e., arglist$main)
#' @param nz The number of images/plots to display
#' @noRd
auto_main_setup = function(arglist_main, nz) {
  if (is.null(arglist_main)) {
    arglist_main = ""
  }
  if (nz != length(arglist_main) & length(arglist_main) != 1) {
    stop("length of main does not match number of plots to construct")
  }
  if (length(arglist_main) == 1) {
    main <- rep(arglist_main, nz)
  } else {
    main <- arglist_main
  }
  return(main)
}

#' Setup zlim argument for autopoints
#'
#' @param arglist_zlim The zlim eleemnt of arglist, i.e., arglist$zlim
#' @param z The z matrix of responses
#' @param common.legend A logical value indicating whether a common legend should be utilized
#' @noRd
autopoints_zlim_setup = function(arglist_zlim, z, common.legend) {
  # if arglist_zlim is null, set to the range of z
  # if there should be a common.legend
  # or a list of the ranges of the columns z if it's not
  # a common legend
  if (is.null(arglist_zlim)) {
    if (common.legend) {
      zlim = range(z, na.rm = TRUE)
    } else {
      zlim = lapply(seq_len(ncol(z)), function(i) {
        range(z[,i], na.rm = TRUE)
      })
    }
  } else {# if arglist_zlim isn't null
    if (!is.list(arglist_zlim) & !is.vector(arglist_zlim)) {
      stop("zlim must be a two-dimensional vector or a list of two-dimensional vectors")
    }
    if (is.list(arglist_zlim)) {
      if (length(arglist_zlim) != ncol(z)) {
        stop("length of zlim list does not match number of plots to construct")
      }
      for (i in seq_along(arglist_zlim)) {
        if (length(arglist_zlim[[i]]) != 2) {
          stop("zlim must be a two-dimensional vector or a list of two-dimensional vectors")
        }
      }
      zlim <- arglist_zlim
    } else {
      if (length(arglist_zlim) != 2) {
        stop("zlim must be a two-dimensional vector or a list of two-dimensional vectors")
      }
      zlim <- rep(list(arglist_zlim), ncol(z))
    }
  }
  return(zlim)
}

#' Setup map information for autoimage, autpoints, etc.
#'
#' @param map A single character string specifying a map
#'            from the maps package
#' @noRd
map_setup = function(map) {
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
      map_lines <- maps::map("county", plot = FALSE)
    } else if (map == "france") {
      utils::data("franceMapEnv", package = "maps")
      map_lines <- maps::map("france", plot = FALSE)
    } else if (map == "nz") {
      utils::data("nzMapEnv", package = "maps")
      map_lines <- maps::map("nz", plot = FALSE)
    } else if (map == "state") {
      utils::data("stateMapEnv", package = "maps")
      map_lines <- maps::map("state", plot = FALSE)
    } else if (map == "usa") {
      utils::data("usaMapEnv", package = "maps")
      map_lines <- maps::map("usa", plot = FALSE)
    } else if (map == "world") {
      utils::data("worldMapEnv", package = "maps")
      map_lines <- maps::map("world", plot = FALSE)
    } else if (map == "world2") {
      utils::data("world2MapEnv", package = "maps")
      map_lines <- maps::map("world2", plot = FALSE)
    } else if (map == "italy") {
      utils::data("italyMapEnv", package = "maps")
      map_lines <- maps::map("italy", plot = FALSE)
    } else if (map == "lakes") {
      utils::data("lakesMapEnv", package = "maps")
      map_lines <- maps::map("lakes", plot = FALSE)
    }
  }
  return(map_lines)
}

#' Setup lines.arg for autoimage, autopoints, ec.
#'
#' @param arglist Argument list
#' @param proj Projection string
#' @noRd
lines_args_setup = function(arglist, proj) {
  # check if there are lines to plot
  arglist_lines <- arglist$lines
  if (!is.null(arglist_lines)) {
    if (!is.list(arglist_lines)) {
      stop("lines must be of class list with vectors x and y")
    }
    if (is.null(arglist_lines$x) | is.null(arglist_lines$y)) {
      stop("lines must be a list with vectors x and y")
    }
    if (length(arglist_lines$x) != length(arglist_lines$y)) {
      stop("The x and y vectors in lines must have he same length")
    }
  }
  # clip lines beyond xlim and ylim if !is.null(arglist_lines)
  if (!is.null(arglist_lines)) {
    arglist_lines$x[arglist_lines$x < arglist$xlim[1]] <- NA
    arglist_lines$x[arglist_lines$x > arglist$xlim[2]] <- NA
    if (proj == "mercator" & arglist$xlim[2] > 179.35) {
      arglist_lines$x[arglist_lines$x > 179.35] <- NA
    }
    arglist_lines$y[arglist_lines$y < arglist$ylim[1]] <- NA
    arglist_lines$y[arglist_lines$y > arglist$ylim[2]] <- NA
  }
  lines.args <- arglist$lines.args
  lines.args$proj <- proj
  lines.args$x <- arglist_lines
  return(lines.args)
}

#' Setup text.args for autoimage, autopoints functions
#'
#' @param arglist Argument list
#' @param proj Projection string
#' @noRd
text_args_setup = function(arglist, proj) {
  text <- arglist$text
  arglist$text <- NULL
  if (!is.null(text)) {
    if (!is.list(text)) {
      stop("text must be a list")
    }
    if (is.null(text$x) | is.null(text$y)) {
      stop("text must be a list with vectors x, y, and (possibly) labels")
    }
    if (length(text$x) != length(text$y)) {
      stop("The x and y vectors in text should have the same length")
    }
    if (is.null(text$labels)) {
      text$labels <- seq_along(text$x)
    }
    if (length(text$x) != length(text$labels)) {
      stop("The x, y, and labels vectors in text should have the same length")
    }
    if (is.data.frame(text)) {
      text <- as.list(text)
    }
  }
  text.args <- arglist$text.args
  arglist$text.args <- NULL
  if (!is.null(text.args)) { 
    if (!is.list(text.args)) {
      stop("text.args must be a list")
    }
  }
  text.args$proj <- proj
  text.args$x <- text$x
  text.args$y <- text$y
  text.args$labels <- text$labels
  return(text.args)
}

#' Setup paxes.args for autoimage, autopoints, ets
#'
#' @param arglist List of arguments
#' @param proj projection
#' @noRd
paxes_args_setup = function(arglist, proj) {
  paxes.args <- arglist$paxes.args
  paxes.args$xlim <- arglist$xlim
  paxes.args$ylim <- arglist$ylim
  paxes.args$xaxp <- arglist$xaxp
  paxes.args$yaxp <- arglist$yaxp
  paxes.args$proj <- proj
  paxes.args$axis.args <- arglist$axis.args
  return(paxes.args)
}

#' Setup points.args for autoimage, autpoints, etc.
#'
#' @param arglist Argument list
#' @param proj Projection
#' @noRd
points_args_setup = function(arglist, proj) {
  # check if there are points to plot
  arglist_points <- arglist$points
  if (!is.null(arglist_points)) {
    if (!is.list(arglist_points)) {
      stop("points must be a list with vectors x and y")
    }
    if (is.null(arglist_points$x) | is.null(arglist_points$y)) {
      stop("points must be a list with vectors x and y")
    }
    if (length(arglist_points$x) != length(arglist_points$y)) {
      stop("The x and y vectors in points should have the same length")
    }
  }
  points.args <- arglist$points.args
  points.args$proj <- proj
  points.args$x <- arglist_points
  return(points.args)
}

#' Setup axes
#'
#' @param arglist List of arguments
#' @noRd
axes_setup = function(arglist) {
  if (is.null(arglist$axes)) {
    axes <- TRUE
  } else {
    axes <- arglist$axes
  }
  return(axes)
}

#' Clear arglist of information no longer needed
#'
#' @param arglist List of arguments
#' @noRd
arglist_clean = function(arglist, image = FALSE) {
  arglist$points.args <- NULL
  arglist$points <- NULL
  arglist$lines <- NULL
  arglist$lines.args <- NULL
  arglist$text <- NULL
  arglist$text.args <- NULL
  arglist$paxes.args <- NULL  
  arglist$axis.args <- NULL
  # will plot axes manually, if necessary
  arglist$axes <- FALSE
  arglist$legend.mar <- NULL
  arglist$interp.args <- NULL
  arglist$legend.axis.args <- NULL
  arglist$border_col <- NULL
  # remove zlim and breaks since not relevant for plot
  if (!image) {
    arglist$zlim <- NULL
    arglist$breaks <- NULL
  }
  return(arglist)
}

#' Setup zlim and breaks (try to make pretty based on n) for
#' a single plot
#'
#' @param arglist Argument list
#' @param n Desired number of color partitions
#' @param ranze_z Range of z
#' @noRd
zlim_breaks_setup = function(zlim, breaks, n, range_z, col) {
  if (is.null(zlim) & is.null(breaks)) {
    breaks <- pretty(range_z, n = n)
    if (!is.null(col)) {
      if (length(breaks) != n + 1) {
        warning("Number of partitions provided by pretty function does not match length(col). Automatically adjusting axis tick labels. User may need to manually specify breaks to improve appearance.")
        breaks <- seq(range_z[1], range_z[2], length = n + 1)
      }
    }
    zlim <- range(breaks, na.rm = TRUE)
  } else if (is.null(zlim) & !is.null(breaks)) {
    if (is.list(breaks)) {
      stop("breaks should not be a list when common.legend = TRUE")
    }
    zlim <- range(breaks, na.rm = TRUE)
  } else if (!is.null(zlim) & is.null(breaks)) {
    if (is.list(zlim)) {
      stop("zlim should not be a list when common.legend = TRUE")
    }
    if (length(zlim) != 2) {
      stop("zlim should specify the minimum and maximum values of z")
    }
    if (!is.numeric(zlim)) {
      stop("zlim must consist of numeric values")
    }
    breaks <- seq(zlim[1], zlim[2], length = n + 1)
  } else if (!is.null(zlim) & !is.null(breaks)) {
    if (min(zlim) != min(breaks)) {
      stop("min of breaks and zlim should match if both are specified")
    }
    if (max(zlim) != max(breaks)) {
      stop("max of breaks and zlim should match if both are specified")
    }
  }
  return(list(zlim = zlim, breaks = breaks))
}

#' Setup zlim and breaks (try to make pretty based on n) for
#' a multiple plots
#'
#' @param arglist Argument list
#' @param n Desired number of color partitions
#' @param ranze_z Range of z
#' @noRd
auto_zlim_breaks_setup = function(arglist, n, z, common.legend) {
  if (common.legend) {
    zlim_breaks <- zlim_breaks_setup(arglist$zlim, arglist$breaks, n, range(z, na.rm = TRUE), arglist$col)
    arglist$zlim = rep(list(zlim_breaks$zlim), ncol(z))
    arglist$breaks = rep(list(zlim_breaks$breaks), ncol(z))
  } else {
    arglist_zlim <- arg_check_zlim(arglist$zlim, ncol(z))
    arglist_breaks <- arg_check_breaks(arglist$breaks, ncol(z))
    for (i in seq_len(ncol(z))) {
      zlim_breaks <- zlim_breaks_setup(arglist_zlim[[i]],
                                       arglist_breaks[[i]],
                                       n,
                                       range(z[,i], na.rm = TRUE),
                                       arglist$col)
      arglist_zlim[[i]] <- zlim_breaks$zlim
      arglist_breaks[[i]] <- zlim_breaks$breaks
    }
    arglist$zlim <- arglist_zlim
    arglist$breaks <- arglist_breaks
  }
  return(arglist)
}

#' Check zlim argument for autopoints
#'
#' @param arglist_zlim arglist$zlim
#' @param nz Number of columns of z
#' @noRd
arg_check_zlim = function(arglist_zlim, nz) {
  if (is.list(arglist_zlim)) {
    if (length(arglist_zlim) != nz) {
      stop("length(zlim) != ncol(z)")
    }
  } else {
    arglist_zlim = rep(list(arglist_zlim), nz)
  }
  return(arglist_zlim)
}

#' Check breaks argument for autopoints
#'
#' @param arglist_breaks arglist$breaks
#' @param nz Number of columns of z
#' @noRd
arg_check_breaks = function(arglist_breaks, nz) {
  if (is.list(arglist_breaks)) {
    if (length(arglist_breaks) != nz) {
      stop("length(breaks) != ncol(z)")
    }
  } else {
    arglist_breaks = rep(list(arglist_breaks), nz)
  }
  return(arglist_breaks)
}


#' Setup col for pimage
#'
#' @param arglist List of additional arguments
#' @return Updated arglist
#' @noRd
pimage_col_arglist_setup <- function(arglist) {
  if (is.null(arglist$col) & is.null(arglist$breaks)) {
    arglist$col <- colorspace::sequential_hcl(n = 64, palette = "Viridis")
  } else if (is.null(arglist$col) & !is.null(arglist$breaks)) {
    nb <- length(arglist$breaks)
    arglist$col <- colorspace::sequential_hcl(n = nb - 1, palette = "Viridis")
  }
  return(arglist)
}

#' Setup legend.scale.args
#'
#' @param arglist List of arguments
#' @return List of legend.scale.args
#' @noRd
legend_scale_args_setup <- function(arglist) {
  legend.scale.args <- list()
  legend.scale.args$zlim <- arglist$zlim
  legend.scale.args$col <- arglist$col
  legend.scale.args$breaks <- arglist$breaks
  legend.scale.args$axis.args <- arglist$legend.axis.args
  return(legend.scale.args)
}

#' Interpolate z for pimage, if necessary
#'
#' @param x vector of x coordinates
#' @param y vector of y coordinates
#' @param z vector of z values
#' @param arglist List of additional arguments
#' @return List of interpolated x, y, and z
#' @noRd
interp_setup <- function(x, y, z, arglist) {
  # setup interpolation arguments
  interp.args <- arglist$interp.args
  # remove non-graphical arguments, if provided
  arglist$interp.args <- NULL
  if (is.null(interp.args)) {
    interp.args <- list()
  }
  
  
  interp.args$xyz <- cbind(x, y, z)
  
  # convert from old format
  if (!is.null(interp.args$xo)) {
    warning("MBA::mba.surf is now used for prediction on a grid instead of the akima::interp function.  Attempting to automatically translate arguments.  Results may slightly differ from previous versions of the package.")
    interp.args$no.X <- length(interp.args$xo)
    interp.args$xo <- NULL
  }
  if (!is.null(interp.args$yo)) {
    warning("MBA::mba.surf is now used for prediction on a grid instead of the akima::interp function.  Attempting to automatically translate arguments.  Results may slightly differ from previous versions of the package.")
    interp.args$no.Y <- length(interp.args$yo)
    interp.args$yo <- NULL
  }
  if (!is.null(interp.args$linear)) {
    warning("MBA::mba.surf is now used for prediction on a grid instead of the akima::interp function.  Attempting to automatically translate arguments.  Results may slightly differ from previous versions of the package.")
    interp.args$linear <- NULL
  }
  if (!is.null(interp.args$extrap)) {
    warning("MBA::mba.surf is now used for prediction on a grid instead of the akima::interp function.  Attempting to automatically translate arguments.  Results may slightly differ from previous versions of the package.")
    interp.args$extend <- interp.args$extrap
    interp.args$extrap <- NULL
  }
  if (!is.null(interp.args$nx)) {
    warning("MBA::mba.surf is now used for prediction on a grid instead of the akima::interp function.  Attempting to automatically translate arguments.  Results may slightly differ from previous versions of the package.")
    interp.args$no.X <- interp.args$nx
    interp.args$nx <- NULL
  }
  if (!is.null(interp.args$ny)) {
    warning("MBA::mba.surf is now used for prediction on a grid instead of the akima::interp function.  Attempting to automatically translate arguments.  Results may slightly differ from previous versions of the package.")
    interp.args$no.Y <- interp.args$ny
    interp.args$ny <- NULL
  }
  
  if (is.null(interp.args$no.X)) {
    interp.args$no.X <- 40
  }
  if (is.null(interp.args$no.Y)) {
    interp.args$no.Y <- 40
  }
  interpf <- MBA::mba.surf
  ixyz <- do.call(interpf, interp.args)
  x <- ixyz$xyz.est$x
  y <- ixyz$xyz.est$y
  z <- ixyz$xyz.est$z
  
  # bug fix for old version of MBA package
  if (length(x) != length(y)) {
    if (length(x) != nrow(z)) {
      z <- matrix(c(z), nrow = ncol(z), ncol = nrow(z))    
    }
  }
  return(list(x = x, y = y, z = z))
}

#' Setup x, y, and zlim in arglist
#' @param x vector or matrix of x coordinates
#' @param y vector or matrix of y coordinates
#' @param z vector or matrix of z coordinates
#' @param arglist List of additional arguments
#' @return Updated arglist
#' @noRd
xyzlim_arglist_setup <- function(x, y, z, arglist) {
  # set limits  
  if (is.null(arglist$xlim)) {
    arglist$xlim <- range(x, na.rm = TRUE)
  }
  if (is.null(arglist$ylim)) {
    arglist$ylim <- range(y, na.rm = TRUE)
  }
  if (is.null(arglist$zlim)) {
    arglist$zlim <- range(z, na.rm = TRUE)
  }
  return(arglist)
}



