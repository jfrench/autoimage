# A function to plot image plots for factor variables on a grid
# factor.image = function(x, y, z, col = NULL, legend = TRUE, legend.args, char.order, horizontal = TRUE, mratio = 5, project = FALSE, map.grid = FALSE, mmar, legend.mar, project.args, grid.args, map.poly, poly.args, map.points, points.args, ...){
#   # obtain elements of ...
#   args = list(...)
#   
#   # sort out x, y, and z.  This is mostly identical to the beginning of
#   # graphics::image
#   if (missing(z)) {
#     if (!missing(x)) {
#       if (is.list(x)) {
#         z <- x$z; y <- x$y; x <- x$x
#       } else {
#         if(is.null(dim(x)))
#           stop("argument must be matrix-like")
#         z <- x
#         x <- seq.int(0, 1, length.out = nrow(z))
#         if(missing(y)) y <- seq.int(0, 1, length.out = ncol(z))
#       }
#       if (is.null(args$xlab)) args$xlab <- ""
#       if (is.null(args$ylab)) args$ylab <- ""
#     } else stop("no 'z' matrix specified")
#   } else if (is.list(x)) {
#     y <- x$y
#     x <- x$x
#   }
# 
#   names = na.omit(unique(z))
#   if(missing(char.order)){
#     char.order = names
#   }else{
#     if(length(char.order) != length(names)) stop("char.order must match length of na.omit(unique(z))")
#   }
#   
#   # convert z to numeric values for plotting
#   znum = matrix(NA, nrow = nrow(zfac), ncol = ncol(zfac))
#   for(i in seq_along(char.order)){
#      which_i = which(zfac == char.order[i], arr.ind = TRUE)
#      znum[which_i] = i
#   }
#   
#   # get x and y labels
#   if(is.null(args$xlab)){
#     args$xlab <- if (missing(x)) "" else deparse(substitute(x))
#   }
#   if(is.null(args$ylab)){
#     args$ylab <- if (missing(y)) "" else deparse(substitute(y))
#   }
#   if(!is.character(z)) stop("z must be a character matrix")
#   if(!is.logical(legend)) stop("legend must be a logical value")
#   # set default arguments for missing arguments
#   if(legend & missing(legend.args)) stop("legend.args must be specified when legend == TRUE")
#   if(missing(mmar)) mmar = par()$mar
#   if(missing(legend.mar)){
#     legend.mar = mmar
#     if(horizontal){
#       legend.mar[3] = 0
#       legend.mar[1] = 3.1
#     }else{
#       legend.mar[2] = 0
#       legend.mar[4] = 3.1
#     }
#   }
#   
#   # specify zlims to match dimensions of z depending on whether
#   # a common legend will be used
#   args$zlim = range(znum, na.rm = TRUE)
#   
#   if(is.null(args$axes)) args$axes = TRUE
#   if(missing(grid.args)) grid.args = list()
#   if(missing(map.poly)) map.poly = NULL
#   if(!is.null(map.poly)){
#     if(!is.list(map.poly)){
#       stop("map.poly must be a list with x and y components")
#     }else{
#       if(is.null(map.poly$x)) stop("The x component of map.poly is missing")
#       if(is.null(map.poly$y)) stop("The y component of map.poly is missing")
#     }
#   }
#   if(missing(map.points)) map.points = NULL
#   if(!is.null(map.points)){
#     if(!is.list(map.points)){
#       stop("map.points must be a list with x and y components")
#     }else{
#       if(is.null(map.points$x)) stop("The x component of map.points is missing")
#       if(is.null(map.points$y)) stop("The y component of map.points is missing")
#     }
#   }
#   
#   if(project){
#     if(is.null(project.args)){
#       project.args = list()
#     }
#     if(!is.matrix(x)){
#       x = matrix(x, nrow = dim(z)[1], ncol = dim(z)[2])
#     }
#     if(!is.matrix(y)){
#       y = matrix(y, nrow = dim(z)[1], ncol = dim(z)[2], byrow = TRUE)
#     }
#     
#     if(map.grid){
#       grid.args$lim = c(range(x), range(y))
#     }
#     if(missing(map.poly)) map.poly = NULL
#     
#     if(is.null(project.args$projection)) project.args$project = ""
#     projectxy = mapproj::mapproject(c(x), c(y), projection = project.args$projection, parameters = project.args$parameters, orientation = project.args$orientation)
#     x = matrix(projectxy$x, nrow = nrow(x))
#     y = matrix(projectxy$y, nrow = nrow(y))
#     if(!is.null(map.poly)){
#       projectpoly = mapproj::mapproject(map.poly$x, map.poly$y)
#       map.poly$x = projectpoly$x
#       map.poly$y = projectpoly$y
#     }
#     if(!is.null(map.points)){
#       projectpoints = mapproj::mapproject(map.points$x, map.points$y)
#       map.points$x = projectpoints$x
#       map.points$y = projectpoints$y
#     }
#   }
#   if(missing(poly.args)) poly.args = list()
#   if(missing(points.args)) points.args = list()
#   if(!is.null(map.poly)){
#     poly.args$x = map.poly$x
#     poly.args$y = map.poly$y
#   }
#   if(!is.null(map.points)){
#     points.args$x = map.points$x
#     points.args$y = map.points$y
#   }
#   
#   # is the grid a regular grid
#   regular = ifelse(length(x) != nrow(z), FALSE, TRUE)
#   # decide plotting function accordingly
#   plotf = fields::poly.image
#   if(regular) plotf = image
#   
#   # choose layout depending on whether a legend is required, and if so,
#   # whether the legend should be horizontal or vertical and whether the legend
#   # is common
#   if(!legend){ # setup if there is no legend
#     mat = matrix(1)
#     lheight = rep(1)
#     lwidth = rep(1)
#   }else{ # setup if there should be a legend
#     if(common.legend){ # setup the legend is common
#       mat = matrix(seq_len(ng), nrow = nr, byrow = TRUE)
#       ni = ng + 1
#       if(horizontal){
#         mat = rbind(mat, matrix(ni, ncol = nc))
#         lheight = c(rep(mratio, nr), 1)
#         lwidth = c(rep(1, nc))
#       }else{
#         mat = cbind(mat, matrix(ni, nrow = nr))
#         lheight = rep(1, nr)
#         lwidth = c(rep(mratio, nc), 1)
#       }
#     }else{ # setup if the legend is not common
#       if(horizontal){ # horizontal legend
#         mat = matrix(0, nrow = 2 * nr, ncol = nc)
#         for(i in seq_len(nr)){
#           crow = (i-1)*2 + 1
#           mat[crow, ] = (i - 1)*nc*2 + seq_len(nc)*2 - 1
#           mat[crow + 1, ] = (i - 1)*nc*2 + seq_len(nc)*2
#         }
#         lheight = c(rep(c(mratio, 1), nr))
#         lwidth = c(rep(1, nc))
#       }else{ # vertical legend
#         mat = matrix(1:(2*ng), nrow = nr, ncol = 2*nc, byrow = TRUE)
#         lheight = rep(1, nr)
#         lwidth = c(rep(c(mratio, 1), nc))
#       }
#     }
#   }
#   
#   curpar = par(no.readonly = TRUE)
#   layout(mat, heights = lheight, widths = lwidth)
#   allmain = args$main
#   allzlim = args$zlim
#   allaxes = args$axes
#   for(i in seq_len(ng)){
#     if(regular){
#       plotargs = list(x = list(x = x, y = y, z = z[,,i]), col = col)
#     }else{
#       plotargs = list(x = x, y = y, z = z[,,i], col = col)
#     }
#     plotargs = c(plotargs, args)
#     plotargs$main = allmain[i]
#     plotargs$zlim = allzlim[[i]]
#     plotargs$axes = FALSE
#     if(project) plotargs$asp = 1
#     par(mar = mmar)
#     do.call(plotf, plotargs)
#     if(!is.null(map.poly)) do.call(graphics::lines, poly.args)
#     if(!is.null(map.points)) do.call(graphics::points, points.args)
#     
#     if(allaxes){
#       xticks = axTicks(1)
#       yticks = axTicks(2)
#       xlabels = TRUE
#       ylabels = TRUE
#       axis(1, at = xticks, labels = xlabels)
#       axis(2, at = yticks, labels = ylabels)
#       box()
#     }
#     if(project & map.grid){
#       do.call(mapproj::map.grid, grid.args)
#     }
#     if(legend & !common.legend){
#       par(mar = legend.mar)
#       legend.scale(zlim = args$zlim[[i]], col = col, horizontal = horizontal, axis.args = axis.args)
#     }
#   }
#   if(legend & common.legend){
#     par(mar = legend.mar)
#     #legend.scale(zlim = args$zlim[[1]], col = col, horizontal = horizontal, axis.args = axis.args)
#     plot(1, axes=FALSE, xlab="", ylab="")
#     do.call(graphics::legend, legend.args)
#   }
#   par(curpar)
# }
