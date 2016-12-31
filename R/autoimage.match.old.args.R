autoimage.match.old.args <- function(legend = "horizontal", proj = "none", 
  proj.args, lratio = 0.2, arglist) {
  if (length(legend) != 1) {
    stop("legend should have length 1")
  }
  if (is.logical(legend)) {
    warning("autoimage has been updated.  Trying to translate arguments to current version")
    if (legend) {
      if (is.null(arglist$horizontal)) {
        legend <- "horizontal"
      } else if (arglist$horizontal) {
        legend <- "horizontal"
      } else {
        legend <- "vertical"
      }
      arglist$horizontal <- NULL
    } else {
      legend <- "none"
    }
  }
  if (length(proj) != 1) {
    stop("proj should be a single character string")
  }
  if (!is.null(arglist$project)) {
    if (!arglist$project) {
      proj <- "none"
    } else {
      proj <- arglist$project.args$projection
      arglist$project.args$projection <- NULL
      proj.args <- arglist$project.args
    }
  }
  if (!is.null(arglist$map.grid)) {
    warning("map.grid has been deprecated.  Consider specifying paxes.args")
    arglist$map.grid <- NULL
  }
  if (!is.null(arglist$map.poly)) {
    warning("map.poly has been deprecated.
  The argument has been renamed \"lines\".
  poly.args has been renamed \"lines.args\".
  Attempting to translate deprecated arguments.")
    arglist$lines <- arglist$map.poly
    arglist$lines.args <- arglist$poly.args
    arglist$map.poly <- NULL
    arglist$poly.args <- NULL
  }
  if (!is.null(arglist$map.points)) {
    warning("map.points has been deprecated.
  The argument has been renamed \"points\".
  Attempting to translate deprecated argument.")
    arglist$points <- arglist$map.points
    arglist$map.points <- NULL
  }
  if (length(lratio) != 1) {
    stop("lratio should be a single positive number")
  }
  
  if (!is.null(arglist$mratio)) {
    warning("mratio has been deprecated.  lratio should be used.
             lratio is the inverse of mratio.
            Attempting to translate deprecated argument.")
    lratio <- 1/arglist$mratio
    arglist$mratio <- NULL
  }
  if (!is.list(proj.args)) {
    stop("proj.args should be a list")
  }
  return(list(legend = legend, proj = proj, proj.args = proj.args, lratio = lratio, 
    arglist = arglist))
}
TRUE
