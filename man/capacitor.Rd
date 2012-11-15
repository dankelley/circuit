\name{capacitor}

\alias{capacitor}

\title{Draw a capacitor}

\description{Draw a capacitor}

\usage{
capacitor(x0, y0, x1, y1,
          points=FALSE,
          length=0.01, width=length*5,  
          label, pos,
          col=par('col'), lwd=par('lwd'), cex=par('cex'))
}

\arguments{
    \item{x0,y0}{starting coordinate of element}
    \item{x1,y1}{ending coordinate of element}
    \item{points}{booline indicating whether to draw dots at the ends of the wire}
    \item{length}{distance between capactor plates}
    \item{width}{length of plates}
    \item{label}{optional string placed near component (see \code{pos})}
    \item{pos}{optional position of \code{label}, as for
        \code{\link{par}(pos)}; defaults to 1 if \code{horizontal=TRUE}, or 2
        otherwise.}
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
resistor(0.1, 0.5, 0.1, 0.8, label="10k")
capacitor(0.4, 0.2, 0.4, 0.5, label='10uF')
wire(0.4, 0.5, 0.7, 0.5)
ground(0.4, 0.2)
}

\author{Dan Kelley}

\keyword{misc}
