#' Add legend to projected image.
#' 
#' \code{autolegend} adds a color scale to the current device based on
#' the information from the last calls to the 
#' \code{\link[autoimage]{autolayout}} and 
#' \code{\link[autoimage]{pimage}} functions.
#' 
#' Internally, \code{autolegend} calls the \code{.legend.scale.args}, 
#' \code{.legend.horizontal}, and \code{.legend.mar} functions to 
#' obtain the relevant information.
#' 
#' @seealso \code{\link[autoimage]{autolayout}}, 
#'   \code{\link[autoimage]{pimage}}, 
#'   \code{\link[autoimage]{legend.scale}}
#' @return NULL
#' @export
#' @examples
#' curpar <- par(no.readonly = TRUE) # get default options
#' data(narccap)
#' autolayout(c(1, 1), legend = 'h')
#' pimage(lon, lat, tasmax[,,1], legend = 'none')
#' autolegend()
#' 
#' # common legend with distinct lines
#' autolayout(c(1, 2), legend = 'h')
#' pimage(lon, lat, tasmax[,,1], legend = 'none', map = 'world')
#' pimage(lon, lat, tasmax[,,2], legend = 'none', map = 'usa', 
#'        proj = 'bonne', proj.args = list(parameters = 40))
#' autolegend()
#' 
#' # separate legends with distinct lines
#' autolayout(c(1, 2), legend = 'v', common.legend = FALSE)
#' pimage(lon, lat, tasmax[,,1], legend = 'none', map = 'state',
#'        proj = 'bonne', proj.args = list(parameters = 40), 
#'        axes = FALSE)
#' autolegend()
#' pimage(lon, lat, tasmax[,,2], legend = 'none', map = 'usa', 
#'        proj = 'albers', proj.args = list(parameters = c(32, 45)), 
#'        axes = FALSE)
#' autolegend()
#' par(curpar) # reset default options
autolegend <- function() {
  legend.mar <- .legend.mar()
  legend.scale.args <- .legend.scale.args()
  if (is.null(legend.scale.args$zlim)) {
    stop("pimage must be called before autolegend")
  }
  legend.scale.args$horizontal <- .legend.horizontal()
  curmar <- par("mar")
  par(mar = legend.mar)
  do.call("legend.scale", legend.scale.args)
  par(mar = curmar)
}
TRUE
