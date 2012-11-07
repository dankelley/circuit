\name{junction}

\alias{junction}

\title{Draw a junction}

\description{Draw a junction}

\usage{
junction(x, y,
         col=par('col'),
         lwd=par('lwd'),
         cex=par('cex'))}

\arguments{
    \item{x}{x coordinate of junction}
    \item{y}{y coordinate of junction}
    \item{col}{colour}
    \item{lwd}{line width}
    \item{cex}{size of points (ignored unless \code{points} is \code{TRUE})}
}

\details{End-point junctions can also be drawn with the \code{points} argument
to e.g. \code{\link{resistor}}.}

\value{None.}

\seealso{The plot must first be set up with \code{\link{circuit}}.}

\examples{
library(circuit)
circuit()
resistor(0.1, 0.5, 0.3, label="10k")
capacitor(0.4, 0.2, 0.3, label='10uF', horizontal=FALSE)
wire(0.4, 0.5, 0.3)
ground(0.4, 0.2)
junction(0.4, 0.5)
}

\author{Dan Kelley}

\keyword{misc}
