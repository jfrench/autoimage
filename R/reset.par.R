#' Reset par
#' 
#' \code{reset.par} resets the arguments of
#' \code{\link[graphics]{par}} to the default values when first
#' opening R (as of version 3.2.2).
#' @seealso \code{\link[graphics]{par}}
#' @return NULL
#' @examples
#' par("mar") #current values of mar
#' par(mar = c(0, 0, 0, 0)) # change values of mar
#' par("mar") # changed values of mar
#' reset.par() # reset to defaults (not necessarilly current values)
#' par("mar") # should be c(5.1, 4.1, 4.1, 2.1)
#' @export
reset.par <- function() {
  parlist <- list(
    xlog = FALSE,
    ylog = FALSE,
    adj = 0.5,
    ann = TRUE,
    ask = FALSE,
    bg = "white",
    bty = "o",
    cex = 1,
    cex.axis = 1,
    cex.lab = 1,
    cex.main = 1.2,
    cex.sub = 1,
    col = "black",
    col.axis = "black",
    col.lab = "black",
    col.main = "black",
    col.sub = "black",
    crt = 0,
    err = 0,
    family = "",
    fg = "black",
    fig = c(0, 1, 0, 1),
    fin = c(6.239583, 5.6875),
    font = 1,
    font.axis = 1,
    font.lab = 1,
    font.main = 2,
    font.sub = 1,
    lab = c(5, 5, 7),
    las = 0,
    lend = "round",
    lheight = 1,
    ljoin = "round",
    lmitre = 10,
    lty = "solid",
    lwd = 1,
    mai = c(1.02, 0.82, 0.82, 0.42),
    mar = c(5.1, 4.1, 4.1, 2.1),
    mex = 1,
    mfcol = c(1, 1),
    mfg = rep(1, 4),
    mfrow = c(1, 1),
    mgp = c(3, 1, 0),
    mkh = 0.001,
    new = FALSE,
    oma = c(0, 0, 0, 0),
    omd = c(0, 1, 0, 1),
    omi = rep(0, 4),
    pch = 1,
    pin = c(4.999583, 3.8475),
    plt = c(0.131419, 0.9326878, 0.1793407, 0.8558242),
    ps = 12,
    pty = "m",
    smo = 1,
    srt = 0,
    tck = NA,
    tcl = -0.5,
    usr = c(0, 1, 0, 1),
    xaxp = c(0, 1, 5),
    xaxs = "r",
    xaxt = "s",
    xpd = FALSE,
    yaxp = c(0, 1, 5),
    yaxs = "r",
    yaxt = "s",
    ylbias = 0.2
  )
  par(parlist)
}
