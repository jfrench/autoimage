#' Creates hidden information for the legend margin and legend scale
#' to using autolegend function.
#' @export
".legend.scale.args" <-
  local({
    val <- list(zlim = NULL, col = NULL, breaks = NULL, axis.args = NULL)
    function(new) if(!missing(new)) val <<- new else val
  })

#' @export
".legend.mar" <-
  local({
    val <- par("mar")
    function(new) if(!missing(new)) val <<- new else val
  })

#' @export
".legend.horizontal" <-
  local({
    val <- TRUE
    function(new) if(!missing(new)) val <<- new else val
  })

