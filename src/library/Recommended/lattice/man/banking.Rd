\name{G_banking}
\alias{banking}
\title{Banking}
\description{
  Calculates banking slope
}
\usage{
banking(dx, dy)
}
\arguments{
  \item{dx, dy}{ vector of consecutive x, y differences. }
}

\details{
  \code{banking} is the banking function used when
  \code{aspect = "xy"} in high level Trellis functions. It is usually not
  very meaningful except with \code{xyplot}.  It considers the
  absolute slopes (based on \code{dx} and \code{dy}) and returns a value
  which when adjusted by the panel scale limits will make the median of
  the above absolute slopes correspond to a 45 degree line.

  This function was inspired by the discussion of banking in the
  documentation for Trellis Graphics available at Bell Labs' website
  (see \code{\link{Lattice}}), but is most likely identical to an
  algorithm described by Cleveland et al (see below).  It is
  not clear (to the author) whether this is the algorithm used in
  S-PLUS.  Alternative banking rules, implemented as a similar function,
  can be used as a drop-in replacement by suitably modifying
  \code{lattice.options("banking")}.

}

\examples{

## with and without banking

plot <- xyplot(sunspot.year ~ 1700:1988, xlab = "", type = "l",
               scales = list(x = list(alternating = 2)),
               main = "Yearly Sunspots")
print(plot, position = c(0, .3, 1, .9), more = TRUE)
print(update(plot, aspect = "xy", main = "", xlab = "Year"),
      position = c(0, 0, 1, .3))

## cut-and-stack plot (see also xyplot.ts)

xyplot(sunspot.year ~ time(sunspot.year) | equal.count(time(sunspot.year)), 
       xlab = "", type = "l", aspect = "xy", strip = FALSE,
       scales = list(x = list(alternating = 2, relation = "sliced")),
       as.table = TRUE, main = "Yearly Sunspots")

}


\references{

  Cleveland, William S. and McGill, Marylyn E. and McGill, Robert (1988)
  \dQuote{The Shape Parameter of a Two-variable Graph}, \emph{Journal of
  the American Statistical Association}, \bold{83}, 289--300.

}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{\code{\link{Lattice}}, \code{\link{xyplot}}}
\keyword{dplot}












