<<<<<<< HEAD
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
#' If \code{legend = TRUE}, then a portion of the device is dedicated to a legend. 
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
#' @importFrom fields poly.image
#' @importFrom mapproj map.grid mapproject
#' @importFrom graphics axTicks axis box image layout par points lines mtext
#' @examples
#' # basic 2x2 layout
#' autolayout(c(2, 2))
#' # 3x2 layout with space for legends
#' autolayout(c(3, 2), legend = TRUE)
#' autolayout(c(3, 2), legend = TRUE, horizontal = FALSE)
#' # 3x2 layout with individuals legends
#' autolayout(c(3, 2), legend = TRUE, common.legend = FALSE)
#' autolayout(c(3, 2), legend = TRUE, horizontal = FALSE, common.legend = FALSE)
#' # if outer title is desired
#' autolayout(c(2, 2), outer = TRUE)
#' # reset oma parameters
#' par(oma = c(0, 0, 0, 0))
#' # impact of mratio when legend used
#' autolayout(c(2, 2), legend = TRUE, mratio = 2)
#' autolayout(c(2, 2), legend = TRUE, mratio = 5)
#' @export
autolayout <- function(size, legend = FALSE, horizontal = TRUE, common.legend = TRUE, mratio = 2, outer = FALSE, show = TRUE){
  arg.check.autolayout(size, legend, horizontal, common.legend, outer, show, mratio)
  # number of rows and columns desired
  ng <- prod(size)
  nr <- size[1]
  nc <- size[2]
  # choose layout depending on whether a legend is required, and if so,
  # whether the legend should be horizontal or vertical and whether the legend
  # is common.  A common legend is plotted after all other plots,
  # while individual legends are plotted after each respective plot.
  if(!legend){ # setup if there is no legend
    mat <- matrix(seq_len(ng), nrow = nr, byrow = TRUE)
    lheight <- rep(1, nr) # common height for all panes
    lwidth <- rep(1, nc) # common width for all panes
  }else{ # setup if there should be a legend
    if(common.legend){ # setup the legend is common
      mat <- matrix(seq_len(ng), nrow = nr, byrow = TRUE)
      if(horizontal){
        # make sure legend is in last position
        mat <- rbind(mat, matrix(ng + 1, ncol = nc)) 
        lheight <- c(rep(mratio, nr), 1)
        lwidth <- c(rep(1, nc))
      }else{
        mat <- cbind(mat, matrix(ng + 1, nrow = nr)) # make sure legend is in first position
        lheight <- rep(1, nr)
        lwidth <- c(rep(mratio, nc), 1)
      }
    }else{ # setup if the legend is not common
      if(horizontal){ # horizontal legend
        mat <- matrix(0, nrow = 2 * nr, ncol = nc)
        for(i in seq_len(nr)){
          crow <- (i-1)*2 + 1
          mat[crow, ] <-  (i - 1)*nc*2 + seq_len(nc)*2 - 1
          mat[crow + 1, ] <- (i - 1)*nc*2 + seq_len(nc)*2
=======
autolayout <- function(size, legend = FALSE, common.legend = TRUE, outer = FALSE, show = FALSE){
  arg.check.autolayout(size, legend, common.legend, outer, show)
  # number of rows and columns desired
  nr = size[1]
  nc = size[2]
  # choose layout depending on whether a legend is required, and if so,
  # whether the legend should be horizontal or vertical and whether the legend
  # is common.  Note that the layout is chosen so that the legends are plotted first
  # then the images, so that lines and points can be added to the image
  # afterward.
  if(!legend){ # setup if there is no legend
    mat = matrix(seq_len(ng), nrow = nr, byrow = TRUE)
    lheight = rep(1, nr) # common height for all panes
    lwidth = rep(1, nc) # common width for all panes
  }else{ # setup if there should be a legend
    if(common.legend){ # setup the legend is common
      mat = matrix(seq_len(ng), nrow = nr, byrow = TRUE)
      # ni = ng + 1
      if(horizontal){
        mat = rbind(mat + 1, matrix(1, ncol = nc)) # make sure legend is in first position
        lheight = c(rep(mratio, nr), 1)
        lwidth = c(rep(1, nc))
      }else{
        mat = cbind(mat + 1, matrix(1, nrow = nr)) # make sure legend is in first position
        lheight = rep(1, nr)
        lwidth = c(rep(mratio, nc), 1)
      }
    }else{ # setup if the legend is not common
      if(horizontal){ # horizontal legend
        mat = matrix(0, nrow = 2 * nr, ncol = nc)
        for(i in seq_len(nr)){
          crow = (i-1)*2 + 1
          mat[crow, ] = (i - 1)*nc*2 + seq_len(nc)*2 # (i - 1)*nc*2 + seq_len(nc)*2 - 1
          mat[crow + 1, ] = (i - 1)*nc*2 + seq_len(nc)*2 - 1 # (i - 1)*nc*2 + seq_len(nc)*2
>>>>>>> 9dd97fb074c91aaac03a524deddc815b24883f99
        }
        lheight = c(rep(c(mratio, 1), nr))
        lwidth = c(rep(1, nc))
      }else{ # vertical legend
<<<<<<< HEAD
        mat <- matrix(1:(2*ng), nrow = nr, ncol = 2*nc, byrow = TRUE)
        lheight <- rep(1, nr)
        lwidth <- c(rep(c(mratio, 1), nc))
      }
    }
  }
  
  # if(!legend){ # setup if there is no legend
  #   mat <- matrix(seq_len(ng), nrow = nr, byrow = TRUE)
  #   lheight <- rep(1, nr) # common height for all panes
  #   lwidth <- rep(1, nc) # common width for all panes
  # }else{ # setup if there should be a legend
  #   if(common.legend){ # setup the legend is common
  #     mat <- matrix(seq_len(ng), nrow = nr, byrow = TRUE)
  #     # ni = ng + 1
  #     if(horizontal){
  #       mat <- rbind(mat + 1, matrix(1, ncol = nc)) # make sure legend is in first position
  #       lheight <- c(rep(mratio, nr), 1)
  #       lwidth <- c(rep(1, nc))
  #     }else{
  #       mat <- cbind(mat + 1, matrix(1, nrow = nr)) # make sure legend is in first position
  #       lheight <- rep(1, nr)
  #       lwidth <- c(rep(mratio, nc), 1)
  #     }
  #   }else{ # setup if the legend is not common
  #     if(horizontal){ # horizontal legend
  #       mat <- matrix(0, nrow = 2 * nr, ncol = nc)
  #       for(i in seq_len(nr)){
  #         crow <- (i-1)*2 + 1
  #         mat[crow, ] <-(i - 1)*nc*2 + seq_len(nc)*2 # (i - 1)*nc*2 + seq_len(nc)*2 - 1
  #         mat[crow + 1, ] <- (i - 1)*nc*2 + seq_len(nc)*2 - 1 # (i - 1)*nc*2 + seq_len(nc)*2
  #       }
  #       lheight = c(rep(c(mratio, 1), nr))
  #       lwidth = c(rep(1, nc))
  #     }else{ # vertical legend
  #       mat <- matrix(1:(2*ng), nrow = nr, ncol = 2*nc, byrow = TRUE) + matrix(rep(c(1, -1), length = ng), nrow = nr, ncol = 2*nc, byrow = TRUE) # the second adjustment is to plot the legend before the image
  #       lheight <- rep(1, nr)
  #       lwidth <- c(rep(c(mratio, 1), nc))
  #     }
  #   }
  # }
  
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

arg.check.autolayout <- function(size, legend, horizontal, common.legend, outer, show, mratio){
=======
        mat = matrix(1:(2*ng), nrow = nr, ncol = 2*nc, byrow = TRUE) + matrix(rep(c(1, -1), length = ng), nrow = nr, ncol = 2*nc, byrow = TRUE) # the second adjustment is to plot the legend before the image
        lheight = rep(1, nr)
        lwidth = c(rep(c(mratio, 1), nc))
      }
    }
  }
  if(show){
    
  }
}

arg.check.autolayout <- function(size, legend, common.legend, outer, show){
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
    stop("legend should be a single logical value")
  }
  if(!is.logical(legend)){
    stop("legend should be a single logical value")
  }
  if(length(horizontal) > 1){
    stop("horizontal should be a single logical value")
  }
  if(!is.logical(horizontal)){
    stop("horizontal should be a single logical value")
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
}