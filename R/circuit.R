circuit <- function(grid=FALSE, mar=rep(2, 4))
{
    plot.new()
    par(mar=mar, mgp=c(2, 0.7, 0))
    plot.window(c(0, 1), c(0, 1), asp=1)
    if (grid) {
        abline(h=seq(-2, 2, 0.1), lty='dotted', col='gray')
        abline(v=seq(-2, 2, 0.1), lty='dotted', col='gray')
        axis(1)
        axis(2)
        box()
    }
}

