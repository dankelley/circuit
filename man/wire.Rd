\name{wire}

\alias{wire}

\title{Draw a wire}

\description{Draw a wire}

\usage{
wire(x0, y0,
     wirelength=0.1, 
     horizontal=TRUE,
     points=FALSE,
     col=par('col'),
     lwd=par('lwd'),
     cex=par('cex'))

}

\arguments{
    \item{x0}{starting x coordinate of element}
    \item{y0}{starting y coordinate of element}
    \item{wirelength}{length of wire plus element}
    \item{horizontal}{boolean indicating whether the wire is drawn horizontally}
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
resistor(0.1, 0.5)
capacitor(0.5, 0.1)
wire(0.5, 0.5)
}

\author{Dan Kelley}

\keyword{misc}
