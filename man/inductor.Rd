\name{inductor}

\alias{inductor}

\title{Draw an inductor}

\description{Draw an inductor}

\usage{
inductor(x0, y0,
         wirelength=0.2, 
         length=0.08,
         width=length/4,  
         horizontal=TRUE,
         label, pos,
         points=FALSE,
         col=par('col'),
         lwd=par('lwd'),
         cex=par('cex'))
}

\arguments{
    \item{x0}{starting x coordinate of element}
    \item{y0}{starting y coordinate of element}
    \item{wirelength}{length of wire plus element}
    \item{length}{length of zig-zag}
    \item{width}{width of zig-zag}
    \item{horizontal}{boolean indicating whether the wire is drawn horizontally}
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

\value{None.}

\seealso{The plot must first be set up with \code{\link{circuit}}.}

\examples{
library(circuit)
circuit()
resistor(0.1, 0.5, 0.3, label="10kohm")
inductor(0.4, 0.2, 0.3, label="10uH", horizontal=FALSE)
wire(0.4, 0.5, 0.3)
ground(0.4, 0.2)
}

\author{Dan Kelley}

\keyword{misc}
