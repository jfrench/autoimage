#' Automatically select plot matrix dimensions
#' 
#' \code{autosize} automatically makes a sensible choice for the 
#' dimensions of a plot matrix based on \code{n}, the number of plots.
#' Only works for \code{n <= 36}. The dimensions are chosen to be as 
#' close to a square as possible.
#' 
#' @param n The number of plots.  Should be a positive integer.
#' @return A vector of length 2 with the number of rows and number of 
#'   columns for the plot matrix.
#' @examples
#' autosize(3)
#' autosize(9)
#' autosize(11)
#' autosize(24)
#' @export
autosize <- function(n) {
  if (length(n) != 1) 
    stop("n should be a single positive integer")
  if (length(n) < 1) 
    stop("n should be a single positive integer")
  n <- round(n)  # just in case somebody puts in a non-integer
  
  if (n == 1) {
    # the trivial case
    return(c(1, 1))
  } else if (n == 2 | n == 3) {
    return(c(1, n))
  } else if (n == 4) {
    return(c(2, 2))
  } else if (n == 5 | n == 6) {
    return(c(2, 3))
  } else if (n > 6 & n <= 9) {
    return(c(3, 3))
  } else if (n > 9 & n <= 12) {
    return(c(3, 4))
  } else if (n > 12 & n <= 16) {
    return(c(4, 4))
  } else if (n > 16 & n <= 20) {
    return(c(4, 5))
  } else if (n > 21 & n <= 25) {
    return(c(5, 5))
  } else if (n > 26 & n <= 30) {
    return(c(5, 6))
  } else if (n > 31 & n <= 36) {
    return(c(6, 6))
  } else {
    stop("autosize only works for n <= 36.  user must choose size.")
  }
}
TRUE
