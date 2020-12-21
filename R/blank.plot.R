#' Draw blank plot
#' 
#' \code{blank.plot} draws a blank plot (no data, axis, or labels) on 
#' the current device.
#' 
#' Used by the \code{autoimage} function to fill remaining regions 
#' with white space when there are more plotting regions than images 
#' to plot.
#' 
#' @seealso \code{autoimage}
#' @return NULL
#' @examples
#' blank.plot()
#' @export
blank.plot <- function() {
  curmar <- par()$mar
  par(mar = rep(0, 4))
  graphics::plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
  par(mar = curmar)
}
