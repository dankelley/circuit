\name{circuit}
\alias{circuit}
\title{Initialize a circuit plot}
\description{Initialize a circuit plot}
\usage{circuit(grid=FALSE, mar=rep(2, 4))}

\arguments{
    \item{grid}{draw a grid and axes}
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
resistor(.1, .5, .3, label="10k")
capacitor(.4, .2, .3, horizontal=FALSE, label="10mF")
}

\author{Dan Kelley}

\keyword{misc}
