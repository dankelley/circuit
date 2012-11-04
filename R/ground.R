ground<- function(x0, y0,
                  length=0.02,         # top line length
                  col=par('col'),
                  lwd=par('lwd'))
{
    lhs <- x0 - length / 2
    rhs <- x0 + length / 2
    y <- y0
    segments(x0 - length / 2, y, x0 + length / 2, y, col=col, lwd=lwd)
    y <- y - length / 5
    segments(x0 - length / 4, y, x0 + length / 4, y, col=col, lwd=lwd)
    y <- y - length / 5
    segments(x0 - length / 10, y, x0 + length / 10, y, col=col, lwd=lwd)
}

