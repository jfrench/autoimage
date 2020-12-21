#' @name inarccap
#' @aliases ilon ilat itasmax
#' @title Interpolated maximum daily surface air temperatures on a regular grid.
#' @description These data are the \code{narccap} data interpolated onto a regular 
#' 140\eqn{\times}115 grid using the \code{akima::interp} function.
#' @docType data
#' @usage data(inarccap)
#' @seealso \code{\link[autoimage]{narccap}}, \code{akima::interp}
#' @format Contains: \describe{ 
#' \item{ilon}{A vector of of longitude coordinates.} 
#' \item{ilat}{A vector of latitude coordinates.} 
#' \item{itasmax}{A 140\eqn{\times}115\eqn{\times}5 array of tasmax values.} }
NULL
