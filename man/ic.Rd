\name{ic}

\alias{ic}

\title{Draw an ic}

\description{Draw an ic}

\usage{ic(x0, y0, x1, y1,
   type = c("555"),
   col=par('col'), lwd=par('lwd'), cex=par('cex'))
}

\arguments{
    \item{x0,y0}{coordinate of lower-left corner of element}
    \item{x1,y1}{coordinate of upper-right corner of element}
    \item{type}{type of IC}
    \item{col}{colour}
    \item{lwd}{line width}
    \item{cex}{size of points (ignored unless \code{points} is \code{TRUE})}
}

\details{The coordinate system has both x and y ranging from 0 to 1.}

\value{A list containing pin information; elements are coordinates \code{x} and
    \code{y}, label \code{label}, number \code{number}, and position
    \code{pos}.}

\seealso{The plot must first be set up with \code{\link{circuit}}.}

\examples{
## An oscillator built using a 555 IC.
library(circuit)
circuit()
p <- ic(0.3, 0.2, 0.5, 0.5)
ctrl <- p[[5]]
gnd <- p[[1]]
trig <- p[[2]]
out <- p[[3]]
res <- p[[4]]
thr <- p[[6]]
dis <- p[[7]]
vcc <- p[[8]]
capacitor(ctrl$x, ctrl$y-0.1, ctrl$x, ctrl$y, label="10nF", pos=4)
wire(gnd$x, gnd$y-0.1, gnd$x, gnd$y)
wire(0.1, 0.1, ctrl$x, 0.1)
label(0.1, 0.1, "GND", pos=2)
wire(out$x, out$y, out$x+0.1, out$y)
label(out$x+0.1, out$y, "VOUT", pos=4)
# top
wire(res$x, res$y, res$x, res$y+0.1)
wire(vcc$x, vcc$y, vcc$x, vcc$y+0.1)
wire(0.1, vcc$y+0.1, vcc$x, vcc$y+0.1)
label(0.1, vcc$y+0.1, "VCC", pos=2)
# lhs components
resistor(0.15, dis$y, 0.1, 0.6, label="R1")
resistor(0.15, 0.3, 0.1, dis$y, label="R2")
wire(0.15, dis$y, dis$x, dis$y)
wire(0.25, thr$y, thr$x, thr$y)
wire(0.25, trig$y, trig$x, trig$y)
wire(0.25, thr$y, 0.25, trig$y-0.05)
wire(0.15, trig$y-0.05, 0.25, trig$y-0.05)
capacitor(0.15, ctrl$y-0.1, 0.15, trig$y-0.05, label="C")
wire(0.15, trig$y-0.05, 0.15, 0.3)
}

\author{Dan Kelley}

\keyword{misc}

