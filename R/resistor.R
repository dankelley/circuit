resistor <- function(x0, y0,
                     wirelength=0.1,   # length of wires plus component
                     length=0.15,      # ziz-zag length
                     width=length/(3 * 2 * sqrt(2)),   # zig-width width
                     horizontal=TRUE,
                     points=FALSE,
                     col=par('col'),
                     lwd=par('lwd'),
                     cex=par('cex'))
{
    if (horizontal) {
        lhs <- x0 + wirelength / 2 - length / 2
        rhs <- x0 + wirelength / 2 + length / 2
        segments(x0, y0, lhs, y0, col=col, lwd=lwd) # wire to component
        segments(rhs, y0, x0+wirelength, y0, col=col, lwd=lwd) # component to wire
        nzigzag <- round(length / width / (2 * sqrt(2)))
        zigzagx <- seq(0, length, length.out=5*nzigzag)
        zigzagy <- rep(c(0, width, 0, -width), length.out=5*nzigzag)
        lines(lhs + zigzagx, y0 + zigzagy)
        if (points) {
            points(x0, y0, pch=20, col=col)
            points(x0+wirelength, y0, pch=20, col=col)
        }
    } else {
        bot <- y0 + wirelength / 2 - length / 2
        top <- y0 + wirelength / 2 + length / 2
        segments(x0, y0, x0, bot, col=col, lwd=lwd) # wire to component
        segments(x0, top, x0, y0+wirelength, col=col, lwd=lwd) # comp to wire
        nzigzag <- round(length / width / (2 * sqrt(2)))
        zigzagx <- rep(c(0, width, 0, -width), length.out=5*nzigzag)
        zigzagy <- seq(0, length, length.out=5*nzigzag)
        lines(x0 + zigzagx, bot + zigzagy)
        if (points) {
            points(x0, y0, pch=20, col=col, cex=cex)
            points(x0, y0+wirelength, pch=20, col=col, cex=cex)
        }
    }
}

