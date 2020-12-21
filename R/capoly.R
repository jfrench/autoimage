#' @name canada
#' @title Provincial and territorial boundaries of Canada, 2001
#' @description An list-like object with components \code{x} and 
#' \code{y} specifying the provincial and territorial boundaries of 
#' Canada during the 2001  
#' census.  The coordinates are in longitude/latitude coordinates.
#' The data was derived from the shapefiles provided by
#' Statistics Canada.  The object also has a component 
#' \code{range} 
#' specifying the range of the data in the order
#' \code{c(min(x), max(x), min(y), max(y))}.  Lastly, the object 
#' has a final component, \code{names}, which provides the name of the region
#' each polygon is associated with.  The object
#' has class \code{map} for compatibility with the \code{maps} package.
#' @docType data
#' @usage data(canada)
#' @format Contains:
#' \describe{
#'  \item{x}{Longitude coordinates for Canadian boundaries}
#'  \item{y}{Latitude coordinates for Canadian boundaries}
#'  \item{range}{Range of x- and y-values}
#'  \item{names}{Region name for each polygon}
#' }
#' @source Shapefile made available by Statistics Canada.  
#' \url{https://www.statcan.gc.ca/eng/start}.
NULL
