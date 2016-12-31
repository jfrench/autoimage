#' Sensible legend margins
#' 
#' \code{automar} determines sensible margins for 
#' \code{\link[autoimage]{legend.scale}} based on the value currently
#' set for \code{par("mar")}.
#' 
#' The margins produced by \code{automar} are based on the current
#' choice of \code{par("mar")}. If the user has specified a poor
#' choice for \code{par("mar")}, then \code{automar} might also
#' produce a poor choice for the legend margin.
#' 
#' @param legend A character string indicating the orientation of the
#'   \code{\link[autoimage]{legend.scale}}. The default is
#'   \code{"none"}.  The other valid options are \code{"horizontal"}
#'   and \code{"vertical"}.
#' @return NULL
#' @examples
#' automar()
#' automar("h")
#' automar("v")
#' @export
automar <- function(legend = "none") {
  legend <- match.arg(legend, c("none", "horizontal", "vertical"))
  legend.mar <- graphics::par("mar")
  if (legend == "horizontal") {
    legend.mar[3] <- 0
    legend.mar[1] <- 3.1
  } else if (legend == "vertical") {
    legend.mar[2] <- 0
    legend.mar[4] <- 3.1
  }
  return(legend.mar)
}
TRUE
