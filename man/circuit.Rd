\name{circuit}
\alias{circuit}
\title{Initialize a circuit plot}
\description{Initialize a circuit plot}
\usage{circuit(xlim=c(0,1), ylim=c(0, 1), axes=FALSE, mar=rep(2, 4))}

\arguments{
    \item{xlim}{limits of x axis}
    \item{ylim}{limits of y axis}
    \item{axes}{draw a grid and axes}
    \item{mar}{margins used in \code{par(mar)}}
}

\details{Initializes a circuit diagram, optionally drawing axes to help in the
    planning of component placement.}

\value{None.}

\seealso{Draw capacitors with \code{\link{capacitor}}, resistors with
    \code{\link{resistor}}, wires with \code{\link{wire}}, and grounds with
    \code{\link{ground}}.}

\examples{
library(circuit)
circuit()
resistor(0.1, 0.5, 0.4, 0.5, label="10k")
capacitor(0.4, 0.2, 0.7, 0.2, label="10mF")
}

\author{Dan Kelley}

\keyword{misc}
