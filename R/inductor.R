inductor <- function(x0, y0, x1, y1,
                     points=FALSE,
                     length=0.08, width=length/4,
                     label, pos,
                     col=par('col'), lwd=par('lwd'), cex=par('cex'))
{
    if (missing(x0)) stop("must provide x0")
    if (missing(y0)) stop("must provide y0")
    if (missing(x1)) stop("must provide x1")
    if (missing(y1)) stop("must provide y1")
    horizontal <- y0 == y1         # FIXME rounding issues?
    wirelength <- if (horizontal) x1 - x0 else y1 - y0
    if (horizontal) {
        lhs <- x0 + wirelength / 2 - length / 2
        rhs <- x0 + wirelength / 2 + length / 2
        segments(x0, y0, lhs, y0, col=col, lwd=lwd) # wire to component
        segments(rhs, y0, x0+wirelength, y0, col=col, lwd=lwd) # component to wire
        ncoil <- 1.5
        coilx <- seq(0, length, length.out=ncoil*20)
        coily <- width * abs(sin(2 * pi * coilx * ncoil / length))
        lines(lhs + coilx, y0 + coily, lwd=2*lwd)
        #browser()
        if (points) {
            points(x0, y0, pch=20, col=col)
            points(x0+wirelength, y0, pch=20, col=col)
        }
        if (!missing(label)) {
            if (missing(pos)) pos <- 1
            if (pos == 1) label(x0 + wirelength / 2, y0 - width/2, label, pos=pos, col=col, cex=cex)
            else if (pos == 3) label(x0 + wirelength / 2, y0 + width/4, label, pos=pos, col=col, cex=cex)
            else label(x0 + wirelength / 2, y0 - width/2, label, pos=pos, col=col, cex=cex)
        }
    } else {
        bot <- y0 + wirelength / 2 - length / 2
        top <- y0 + wirelength / 2 + length / 2
        segments(x0, y0, x0, bot, col=col, lwd=lwd) # wire to component
        segments(x0, top, x0, y0+wirelength, col=col, lwd=lwd) # comp to wire
        ncoil <- 1.5
        coily <- seq(0, length, length.out=ncoil*50)
        coilx <- width * abs(sin(2 * pi * coily * ncoil / length))
        lines(x0 + coilx, bot + coily, lwd=2*lwd)
        if (points) {
            points(x0, y0, pch=20, col=col, cex=cex)
            points(x0, y0+wirelength, pch=20, col=col, cex=cex)
        }
        if (!missing(label)) {
            if (missing(pos)) pos <- 2
            dy <- par('cin')[2] / par('fin')[2] / 4
            if (pos == 2)
                label(x0 - width/4, y0 + wirelength/2 - dy,
                      label, pos=pos, col=col, cex=cex)
            else if (pos == 4)
                label(x0 + width/2, y0 + wirelength/2 - dy,
                      label, pos=pos, col=col, cex=cex)
            else
                label(x0 - width / 2, y0 + wirelength/2,
                      label, pos=pos, col=col, cex=cex)
        }
    }
    invisible(c(x0, y0, x1, y1))
}

