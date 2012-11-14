\name{wire}

\alias{wire}

\title{Draw a wire}

\description{Draw a wire}

\usage{
wire(x0, y0, x1, y1,
     points=FALSE,
     col=par('col'), lwd=par('lwd'), cex=par('cex'))
}

\arguments{
    \item{x0,y0}{starting coordinate of element}
    \item{x1,y1}{ending coordinate of element}
    \item{points}{booline indicating whether to draw dots at the ends of the wire}
    \item{col}{colour}
    \item{lwd}{line width}
    \item{cex}{size of points (ignored unless \code{points} is \code{TRUE})}
}

\details{The coordinate system has both x and y ranging from 0 to 1.}

\value{None.}

\seealso{The plot must first be set up with \code{\link{circuit}}.}

\examples{
library(circuit)
circuit()
resistor(0.1, 0.5, 0.4, 0.5, label="10k")
capacitor(0.4, 0.2, 0.4, 0.5, label='10uF')
wire(0.4, 0.5, 0.7, 0.5)
ground(0.4, 0.2)
}

\author{Dan Kelley}

\keyword{misc}
