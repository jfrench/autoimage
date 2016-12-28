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
  graphics::plot(1:2, 1:2, type = "n", axes = FALSE, ann = FALSE)
}
TRUE
