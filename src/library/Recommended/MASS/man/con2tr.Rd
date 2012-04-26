\name{con2tr}
\alias{con2tr}
\title{
  Convert Lists to Data Frames for use by lattice
}
\description{
  Convert lists to data frames for use by lattice.
}
\usage{
con2tr(obj)
}
\arguments{
  \item{obj}{
    A list of components \code{x}, \code{y} and \code{z} as passed to
    \code{contour}.
  }
}
\value{
  A data frame suitable for passing to lattice (formerly trellis) functions.
}
\details{
  \code{con2tr} repeats the \code{x} and \code{y} components suitably to
  match the vector \code{z}.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\keyword{misc}
