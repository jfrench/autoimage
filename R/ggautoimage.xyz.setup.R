ggautoimage.xyz.setup <- function(x, y, z, f, interp.args) {
  df <- data.frame(x, y, z, f)
  split_df <- split(df, f = f)
  for (i in seq_along(split_df)) {
    xi <- split_df[[i]]$x
    yi <- split_df[[i]]$y
    zi <- split_df[[i]]$z
    fi <- split_df[[i]]$f[1]
    uxi <- unique(xi)
    uyi <- unique(yi)
    # if not on a regular grid, interpolate to regular grid
    if (length(uxi) * length(uyi) != length(zi)) {
      temp.interp.args <- interp.args
      temp.interp.args$x <- c(xi)
      temp.interp.args$y <- c(yi)
      temp.interp.args$z <- c(zi)
      if (requireNamespace("akima", quietly = TRUE)) {
        fun <- akima::interp
      } else {
        stop("User must manually install the akima package to enable this functionality due to licensing restrictions")
      }
      
      # interpoluation output
      iout <- do.call(fun, temp.interp.args)
      xy <- expand.grid(iout$x, iout$y)
      split_df[[i]] <- data.frame(x = xy[, 1], y = xy[, 2], z = c(iout$z), 
        f = fi)
    }
  }
  return(do.call("rbind", split_df))
}
