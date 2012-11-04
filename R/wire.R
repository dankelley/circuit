wire <- function(x0, y0,
                 wirelength=0.1,
                 horizontal=TRUE,
                 points=FALSE,
                 col=par('col'),
                 lwd=par('lwd'),
                 cex=par('cex'))
{
    if (horizontal) {
        segments(x0, y0, x0 + wirelength, y0, col=col, lwd=lwd)
        if (points) {
            points(x0, y0, pch=20, col=col)
            points(x0+wirelength, y0, pch=20, col=col)
        }
    } else {
        segments(x0, y0, x0, y0 + wirelength,  col=col, lwd=lwd)
        if (points) {
            points(x0, y0, pch=20, col=col)
            points(x0, y0 + wirelength, pch=20, col=col)
        }
    }
}

