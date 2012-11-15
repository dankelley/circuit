\name{resistor}

\alias{resistor}

\title{Draw a resistor}

\description{Draw a resistor}

\usage{
resistor(x0, y0, x1, y1,
          points=FALSE,
          length=0.08, width=length/(3 * 2 * sqrt(2)),  
          label, pos,
          col=par('col'), lwd=par('lwd'), cex=par('cex'))
}

\arguments{
    \item{x0,y0}{starting coordinate of element}
    \item{x1,y1}{ending coordinate of element}
    \item{length}{length of zig-zag}
    \item{width}{width of zig-zag}
    \item{label}{optional string placed near component (see \code{pos})}
    \item{pos}{position of \code{label}: 0 for below a horizontal
        component, or to the left of a vertical one, and 1 for
        the opposite}
    \item{points}{booline indicating whether to draw dots at the ends of the wire}
    \item{col}{colour}
    \item{lwd}{line width}
    \item{cex}{size of points (ignored unless \code{points} is \code{TRUE})}
}

\details{The coordinate system has both x and y ranging from 0 to 1.}

\value{Vector containing \code{x0, y0, x1, y1}.}

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
