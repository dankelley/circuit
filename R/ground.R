ground<- function(x0, y0,
                  length=0.04,         # top line length
                  col=par('col'),
                  lwd=par('lwd'))
{
    lhs <- x0 - length / 2
    rhs <- x0 + length / 2
    y <- y0
    segments(x0 - length / 2, y, x0 + length / 2, y, col=col, lwd=2*lwd)
    y <- y - length / 5
    segments(x0 - length / 2 + length / 5, y,
             x0 + length / 2 - length / 5, y,
             col=col, lwd=2*lwd)
    y <- y - length / 5
    segments(x0 - length / 2 + 2*length / 5, y,
             x0 + length / 2 - 2*length / 5, y,
             col=col, lwd=2*lwd)
    y <- y - length / 5
    segments(x0 - length / 2 + 2.5*length / 5, y,
             x0 + length / 2 - 2.5*length / 5, y,
             col=col, lwd=2*lwd)
}

