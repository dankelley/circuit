capacitor <- function(x0, y0,
                      wirelength=0.1,  # includes wires
                      length=0.015,    # plate-plate distance
                      width=length*4,  # plate length
                      horizontal=TRUE,
                      points=FALSE,
                      col=par('col'),
                      lwd=par('lwd'),
                      cex=par('cex'))
{
    if (horizontal) {
        lhs <- x0 + wirelength / 2 - length / 2
        rhs <- x0 + wirelength / 2 + length / 2
        bot <- y0 - width / 2
        top <- y0 + width / 2
        segments(x0, y0, lhs, y0, col=col, lwd=lwd) # wire to component
        segments(rhs, y0, x0+wirelength, y0, col=col, lwd=lwd) # component to wire
        segments(lhs, bot, lhs, top, col=col, lwd=lwd) # component: plate 1
        segments(rhs, bot, rhs, top, col=col, lwd=lwd) # component: plate 2
        if (points) {
            points(x0, y0, pch=20, col=col)
            points(x0+wirelength, y0, pch=20, col=col)
        }
    } else {
        lhs <- x0 - width / 2
        rhs <- x0 + width / 2
        bot <- y0 + wirelength / 2 - length / 2
        top <- y0 + wirelength / 2 + length / 2
        segments(x0, y0, x0, bot, col=col, lwd=lwd)
        segments(x0, top, x0, y0+wirelength, col=col, lwd=lwd)
        segments(lhs, bot, rhs, bot, col=col, lwd=lwd)
        segments(lhs, top, rhs, top, col=col, lwd=lwd)
        if (points) {
            points(x0, y0, pch=20, col=col, cex=cex)
            points(x0, y0+wirelength, pch=20, col=col, cex=cex)
        }
    }
}

