label <- function(x0, y0, label, ...)
{
    if (length(grep("uF", label))) {
        number <- sub("uF", "", label)
        label <- as.expression(substitute(n * mu * unit, list(n=number, unit="F")))
    } else if (length(grep("Ohm", label, ignore.case=TRUE))) {
        number <- sub("Ohm", "", label, ignore.case=TRUE)
        label <- as.expression(substitute(n * Omega, list(n=number)))
    }
    text(x0, y0, label, ...)
    ## FIXME should translate some things
}

