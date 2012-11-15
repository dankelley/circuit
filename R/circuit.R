circuit <- function(xlim=c(0,1), ylim=c(0,1), axes=FALSE, mar=rep(2, 4))
{
    plot.new()
    par(mar=mar, mgp=c(2, 0.7, 0))
    plot.window(xlim, ylim, asp=1)
    if (axes) {
        abline(h=seq(-5, 5, 0.1), lty='dotted', col='gray')
        abline(v=seq(-5, 5, 0.1), lty='dotted', col='gray')
        axis(1)
        axis(2)
        box()
    }
}

