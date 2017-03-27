#' @name copoly
#' @title Colorado state border
#' @description A list-like object with components \code{x} and \code{y} specifying
#' the borders of the state of Colorado in longitude/latitude coordinates.  
#' This was derived from the \code{\link[maps]{stateMapEnv}} data set 
#' in the \code{maps} package.  The object also has a component 
#' \code{range} 
#' specifying the range of the data in the order
#' \code{c(min(x), max(x), min(y), max(y))}.  Lastly, the object 
#' has a final component, \code{names}, which provides names for each
#' polygon.  In this case, the only name is \code{"colorado"}.  The object
#' has class \code{map} for compatibility with the \code{maps} package.
#' @docType data
#' @usage data(copoly)
#' @format Contains:
#' \describe{
#'  \item{x}{longitude coordinates for Colorado border}
#'  \item{y}{latitude coordinates for Colorado border}
#'  \item{range}{Range of x- and y-values}
#'  \item{names}{Name of polygon}
#' }
#' @source The \code{\link[maps]{stateMapEnv}} data set in the 
#' \code{maps} package.
NULL
