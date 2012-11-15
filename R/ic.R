ic <- function(x0, y0, x1, y1,
               type=c("555"),
               col=par('col'), lwd=par('lwd'), cex=par('cex'))
{
    if (missing(x0)) stop("must provide x0")
    if (missing(y0)) stop("must provide y0")
    if (missing(x1)) stop("must provide x1")
    if (missing(y1)) stop("must provide y1")
    type <- match.arg(type)
    if (type == "555") {
    } else {
        stop("unknown chip type")      # this statement cannot be reached
    }
    lines(c(x0, x1, x1, x0, x0), c(y0, y0, y1, y1, y0), col=col, lwd=2*lwd)
    pins <- list(pin1=list(number=1, label="GND", x=x0 + (x1-x0)/3, y=y0, pos=1),
                 pin2=list(number=2, label="TRIG", x=x0, y=y0 + (y1-y0)/4, pos=2),
                 pin3=list(number=3, label="OUT", x=x1, y=0.5*(y0+y1), pos=4),
                 pin4=list(number=4, label="RES", x=x0+(x1-x0)/3, y=y1, pos=3),
                 pin5=list(number=5, label="CTRL", x=x0 + 2*(x1-x0)/3, y=y0, pos=1),
                 pin6=list(number=6, label="TRH", x=x0, y=y0 + 2*(y1-y0)/4, pos=2),
                 pin7=list(number=7, label="DIS", x=x0, y=y0 + 3*(y1-y0)/4, pos=2),
                 pin8=list(number=8, label="VCC", x=x0+2*(x1-x0)/3, y=y1, pos=3))
    for (p in seq_along(pins)) {
        pin <- pins[[p]]
        dx <- 0.5*par('cin')[1]/par('fin')[1]
        dy <- 0.5*par('cin')[2]/par('fin')[2]
        if (pin$pos == 1) {
            text(pin$x, pin$y, pin$label, pos=3, cex=0.7*cex)
            text(pin$x-dx, pin$y-dy, pin$number, cex=0.7*cex)
        } else if (pin$pos == 2) {
            text(pin$x, pin$y, pin$label, pos=4, adj=0.1, cex=0.7*cex)
            text(pin$x-dx, pin$y+dy, pin$number, cex=0.7*cex)
        } else if (pin$pos == 3) {
            text(pin$x, pin$y, pin$label, pos=1, cex=0.7*cex)
            text(pin$x-dx, pin$y+dy, pin$number, cex=0.7*cex)
        } else if (pin$pos == 4) {
            text(pin$x, pin$y, pin$label, pos=2, cex=0.7*cex)
            text(pin$x-dx, pin$y+dy, pin$number, pos=pin$pos, cex=0.7*cex)
        }
        points(pin$x, pin$y, pch=20, cex=cex)
    }
    pins
}

