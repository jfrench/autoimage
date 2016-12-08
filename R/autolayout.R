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
        }
        lheight = c(rep(c(mratio, 1), nr))
        lwidth = c(rep(1, nc))
      }else{ # vertical legend
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
}