\name{label}

\alias{label}

\title{Draw a label}

\description{Draw a label}

\usage{label(x0, y0, label, ...)}

\arguments{
    \item{x0}{topx coordinate of label symbol}
    \item{y0}{top y coordinate of label symbol}
    \item{label}{label to draw (may be an expression)}
    \item{\dots}{other arguments, passed to \code{\link{text}}.}
}

\details{This is a wrapper around a call to \code{\link{text}}, with the
    advantage being that some abbreviations are handled, e.g \code{label="uF"}
    will be shown with a mu symbol for the u, \code{label="10kohm"} will
    be typeset with an Omega symbol, etc.}

\value{None.}

\seealso{The plot must first be set up with \code{\link{circuit}}.}

\examples{
library(circuit)
circuit()
label(0.1, 0.5, "Vin", pos=2)
}

\author{Dan Kelley}

\keyword{misc}
