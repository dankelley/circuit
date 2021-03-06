\name{ground}

\alias{ground}

\title{Draw a ground}

\description{Draw a ground}

\usage{ground(x0, y0,
    length=0.04,
    col=par('col'),
    lwd=par('lwd'))
}

\arguments{
    \item{x0}{topx coordinate of ground symbol}
    \item{y0}{top y coordinate of ground symbol}
    \item{length}{length of ground symbol}
    \item{col}{colour}
    \item{lwd}{line width}
}

\details{The coordinate system has both x and y ranging from 0 to 1.}

\value{None.}

\seealso{The plot must first be set up with \code{\link{circuit}}.}

\examples{
library(circuit)
circuit()
resistor(0.1, 0.5, 0.3, 0.5)
capacitor(0.3, 0.3, 0.3, 0.5)
wire(0.3, 0.5, 0.5, 0.5)
ground(0.3, 0.3)
}

\author{Dan Kelley}

\keyword{misc}
