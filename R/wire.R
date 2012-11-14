wire <- function(x0, y0, x1, y1,
                 points=FALSE,
                 col=par('col'), lwd=par('lwd'), cex=par('cex'))
{
    if (missing(x0)) stop("must provide x0")
    if (missing(y0)) stop("must provide y0")
    if (missing(x1)) stop("must provide x1")
    if (missing(y1)) stop("must provide y1")
    segments(x0, y0, x1, y1, col=col, lwd=lwd)
    if (points) {
        points(x0, y0, pch=20, col=col)
        points(x1, y1, pch=20, col=col)
    }
}

