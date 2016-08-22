#' @name narccap
#' @aliases lon lat tasmax
#' @title Maximum daily surface air temperatures on a grid.
#' @description These data are taken from the North American Regional Climate Change Assessment Program (NARCCAP).  Specifically, it is five consecutive days of maximum daily surface air temperature (K) (abbreviated tasmax) output from the Canadian Regional Climate Model forced by the Community Climate System Model.  
#' @docType data
#' @usage data(narccap)
#' 
#' @format Contains:
#' \describe{
#'  \item{lon}{A 140\eqn{\times}115 matrix of longitude coordinates.}
#'  \item{lat}{A 140\eqn{\times}115 matrix of latitude coordinates.}
#'  \item{tasmax}{A 140\eqn{\times}115\eqn{\times}5 array of tasmax values.}
#' }
#' @source The National Center for Atmospheric Research  \url{http://www.narccap.ucar.edu/index.html}.
#' @references Mearns, L.O., et al., 2007, updated 2012. The North American Regional Climate Change Assessment Program dataset, National Center for Atmospheric Research Earth System Grid data portal, Boulder, CO. Data downloaded 2016-08-12. [doi:10.5065/D6RN35ST] 
#' 
#' Mearns, L. O., W. J. Gutowski, R. Jones, L.-Y. Leung, S. McGinnis, A. M. B. Nunes, and Y. Qian: A regional climate change assessment program for North America. EOS, Vol. 90, No. 36, 8 September 2009, pp. 311-312. 
NULL