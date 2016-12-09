#' Divide device into rows and columns
#'
#' \code{autolayout} divides the current device into equal-sized rows and equal-sized 
#' columns based on the specified arguments.  
#' 
#' The rows and columns are constructed using the 
#' \code{\link[graphics]{layout}} function, which is incompatible with the 
#' \code{mfrow} and \code{mfcol} arguments in the \code{\link[graphics]{par}} 
#' function and is also incompatible with the 
#' \code{\link[graphics]{split.screen}} function.
#' 
#' Note \code{\link[graphics]{par}} parameters are NOT RESET after executing the 
#' \code{\link[graphics]{layout}} function so the the user can use existing layout 
#' for plots.
#'
#' If \code{legend = "horizontal"} or \code{legend = "vertical"}, 
#' then a portion of the device is dedicated to a legend. 
#' 
#' If \code{common.legend = TRUE}, then one legend region is created for the 
#' entire set of plots.  
#' If \code{common.legend = FALSE}, then a separate legend region is created for each 
#' individual plot.
#'
#' With respective to ordering of the plotting regions: A common legend is plotted after 
#' all other plots, while individual legends are plotted after each respective plot.
#'
#' @inheritParams autoimage
#' @param outer A logical value indicating whether the room should be left for an outer title that 
#' is common for all plots.  Depends on setting the \code{oma} argument of the 
#' \code{\link[graphics]{par}} function.
#' @param show A logical value indicating whether the \code{\link[graphics]{layout.show}} function 
#' should be called after the layout is constructed.  
#' @references Portions of the code for this function is inspired by the internals of the \code{\link[fields]{image.plot}} function written by Doug Nychka and from the \code{image.scale.2} function written by Marc Taylor and discussed at \code{http://menugget.blogspot.com/2013/12/new-version-of-imagescale-function.html}.  For compatibility with the \code{\link[graphics]{image}} function, some of the sanity checking and data formatting are taken almost directly from the \code{\link[graphics]{image}} function.
#' @seealso \code{\link[graphics]{image}}, \code{\link[fields]{image.plot}}, \code{\link[graphics]{axis}}
#' @return NULL
#' @examples
#' # basic 2x2 layout
#' autolayout(c(2, 2))
#' # 3x2 layout with space for legends
#' autolayout(c(3, 2), legend = "h")
#' autolayout(c(3, 2), legend = "v")
#' # 3x2 layout with individuals legends
#' autolayout(c(3, 2), legend = "h", common.legend = FALSE)
#' autolayout(c(3, 2), legend = "v", common.legend = FALSE)
#' # if outer title is desired
#' autolayout(c(2, 2), outer = TRUE)
#' # reset oma parameters
#' par(oma = c(0, 0, 0, 0))
#' # impact of mratio when legend used
#' autolayout(c(2, 2), legend = "h", mratio = 2)
#' autolayout(c(2, 2), legend = "h", mratio = 5)

#' @export
autolayout <- function(size, legend = "none", common.legend = TRUE, 
                       mratio = 2, outer = FALSE, show = TRUE, reverse = FALSE){
  # match legend argument
  legend <- try(match.arg(legend, c("none", "horizontal", "vertical")), silent = TRUE)
  # sanity check
  arg.check.autolayout(size, legend, common.legend, outer, show, mratio, reverse)
  # number of rows and columns desired
  ng <- prod(size)
  nr <- size[1]
  nc <- size[2]
  # choose layout depending on whether a legend is required, and if so,
  # whether the legend should be horizontal or vertical and whether the legend
  # is common.  A common legend is plotted after all other plots,
  # while individual legends are plotted after each respective plot.
  if(legend == "none"){ # setup if there is no legend
    mat <- matrix(seq_len(ng), nrow = nr, byrow = TRUE)
    lheight <- rep(1, nr) # common height for all panes
    lwidth <- rep(1, nc) # common width for all panes
  }else{ # setup if there should be a legend
    if(common.legend){ # setup the legend is common
      mat <- matrix(seq_len(ng), nrow = nr, byrow = TRUE)
      if(legend == "horizontal"){
        # make sure legend is in last position
        if(!reverse){
          mat <- rbind(mat, matrix(ng + 1, ncol = nc))
        }else{ #first position
          mat <- rbind(mat + 1, matrix(1, ncol = nc)) # make sure legend is in first position
        }
        lheight <- c(rep(mratio, nr), 1)
        lwidth <- c(rep(1, nc))
      }else{
        if(!reverse){
          mat <- cbind(mat, matrix(ng + 1, nrow = nr)) # make sure legend is in first position
        }else{
          mat <- cbind(mat + 1, matrix(1, nrow = nr)) # make sure legend is in first position  
        }
        lheight <- rep(1, nr)
        lwidth <- c(rep(mratio, nc), 1)
      }
    }else{ # setup if the legend is not common
      if(legend == "horizontal"){ # horizontal legend
        mat <- matrix(0, nrow = 2 * nr, ncol = nc)
        for(i in seq_len(nr)){
          crow <- (i-1)*2 + 1
          if(!reverse){
            mat[crow, ] <-  (i - 1)*nc*2 + seq_len(nc)*2 - 1
            mat[crow + 1, ] <- (i - 1)*nc*2 + seq_len(nc)*2
          }else{
            mat[crow, ] <- (i - 1)*nc*2 + seq_len(nc)*2 
            mat[crow + 1, ] <- (i - 1)*nc*2 + seq_len(nc)*2 - 1
          }
        }
        lheight = c(rep(c(mratio, 1), nr))
        lwidth = c(rep(1, nc))
      }else{ # vertical legend
        mat <- matrix(1:(2*ng), nrow = nr, ncol = 2*nc, byrow = TRUE)
        if(reverse){
          mat <- matrix(1:(2*ng), nrow = nr, ncol = 2*nc, byrow = TRUE) + matrix(rep(c(1, -1), length = ng), nrow = nr, ncol = 2*nc, byrow = TRUE)
        }
        lheight <- rep(1, nr)
        lwidth <- c(rep(c(mratio, 1), nc))
      }
    }
  }
  
  if(outer){
    oma <- par()$oma
    # make sure there's room for outer title
    if(max(oma) == 0){
      warning("There is no room in the outer margin for an outer title.  Setting par(oma = c(0, 0, 3, 0)).")
      par(oma = c(0, 0, 3, 0))
    }
  } 
  # execute layout
  layout(mat, heights = lheight, widths = lwidth)
  # show layout, if desired
  if(show){
    graphics::layout.show(max(mat))
  }
}

arg.check.autolayout <- function(size, legend, common.legend, outer, show, mratio, reverse){
  if(length(size) != 2){
    stop("size should be a vector of length 2")
  }
  if(!is.numeric(size)){
    stop("size should be numeric")
  }
  if(min(size) < 1){
    stop("the elements of size should be positive integers")
  }
  if(length(legend) > 1){
    stop('invalid legend argument.  legend should be "none", "horizontal", or "vertical".')
  }
  if(class(legend) == "try-error"){
    stop('invalid legend argument.  legend should be "none", "horizontal", or "vertical".')
  }
  if(length(common.legend) > 1){
    stop("common.legend should be a single logical value")
  }
  if(!is.logical(common.legend)){
    stop("common.legend should be a single logical value")
  }
  if(length(outer) > 1){
    stop("outer should be a single logical value")
  }
  if(!is.logical(outer)){
    stop("outer should be a single logical value")
  }
  if(length(show) > 1){
    stop("show should be a single logical value")
  }
  if(!is.logical(show)){
    stop("show should be a single logical value")
  }
  if(length(mratio) != 1){
    stop("mratio should be a single positive number")
  }
  if(!is.numeric(mratio)){
    stop("mratio should be a single positive number")
  }
  if(mratio <= 0){
    stop("mratio should be a single positive number")
  }
  if(length(reverse) > 1){
    stop("reverse should be a single logical value")
  }
  if(!is.logical(reverse)){
    stop("reverse should be a single logical value")
  }
}