\name{C_07_shingles}
\alias{equal.count}
\alias{plot.shingle}
\alias{print.shingle}
\alias{as.character.shingleLevel}
\alias{print.shingleLevel}
\alias{summary.shingle}
\alias{as.data.frame.shingle}
\alias{as.factorOrShingle}
\alias{as.shingle}
\alias{[.shingle}
\alias{is.shingle}
\alias{shingle}
\title{shingles}
\usage{
shingle(x, intervals=sort(unique(x)))
equal.count(x, \dots)
as.shingle(x)
is.shingle(x)

\method{plot}{shingle}(x, panel, xlab, ylab, \dots)

\method{print}{shingle}(x, showValues = TRUE, \dots)

\method{as.character}{shingleLevel}(x, \dots)

\method{print}{shingleLevel}(x, \dots)

\method{summary}{shingle}(object, showValues = FALSE, \dots)

% \method{as.data.frame}{shingle}(x, row.names = NULL, optional = FALSE)

\method{[}{shingle}(x, subset, drop = FALSE)
as.factorOrShingle(x, subset, drop)
}
\description{
  Functions to handle shingles
}
\arguments{
  \item{x}{
    numeric variable or R object, shingle in \code{plot.shingle} and
    \code{x[]}. An object (list of intervals) of class "shingleLevel" in
    \code{print.shingleLevel}
  }
  \item{object}{ shingle object to be summarized}
  \item{showValues}{ logical, whether to print the numeric part. If
    FALSE, only the intervals are printed}
%   \item{row.names}{ a character vector giving the row names for the data frame}
%   \item{optional}{
%     logical.  If \code{TRUE}, setting row names is optional
%   }
  \item{intervals}{ numeric vector or matrix with 2 columns}
  \item{subset}{logical vector}
  \item{drop}{whether redundant shingle levels are to be dropped}
  \item{panel, xlab, ylab}{ standard Trellis arguments (see
    \code{\link{xyplot}} ) }
  \item{\dots}{ other arguments, passed down as appropriate.  For
    example, extra arguments to \code{equal.count} are passed on to
    \code{co.intervals}.  graphical parameters can be passed as
    arguments to the \code{plot} method.
  }
}

\details{
  A shingle is a data structure used in Trellis, and is a generalization
  of factors to \sQuote{continuous} variables.  It consists of a numeric
  vector along with some possibly overlapping intervals. These intervals
  are the \sQuote{levels} of the shingle.  The \code{levels} and
  \code{nlevels} functions, usually applicable to factors, also work on
  shingles.  The implementation of shingles is slightly different from
  S.

  There are print methods for shingles, as well as for printing the
  result of \code{levels()} applied to a shingle.  For use in labelling,
  the \code{as.character} method can be used to convert levels of a
  shingle to character strings.

  \code{equal.count} converts \code{x} to a shingle using the equal
  count algorithm.  This is essentially a wrapper around
  \code{co.intervals}.  All arguments are passed to \code{co.intervals}.

  \code{shingle} creates a shingle using the given \code{intervals}. If
  \code{intervals} is a vector, these are used to form 0 length
  intervals.

  \code{as.shingle} returns \code{shingle(x)} if \code{x} is not a
  shingle.

  \code{is.shingle} tests whether \code{x} is a shingle.

  \code{plot.shingle} displays the ranges of shingles via
  rectangles. \code{print.shingle} and \code{summary.shingle} describe
  the shingle object.
}
\value{
  \code{x$intervals} for \code{levels.shingle(x)}, 
  logical for \code{is.shingle}, an object of class \code{"trellis"} for
  \code{plot} (printed by default by \code{print.trellis}), and 
  an object of class \code{"shingle"} for the others.
}
\examples{
z <- equal.count(rnorm(50))
plot(z)
print(z)
print(levels(z))
\testonly{data.frame(x = equal.count(rnorm(100)), y = rnorm(100))}
}
\author{ Deepayan Sarkar \email{Deepayan.Sarkar@R-project.org}}
\seealso{
  \code{\link{xyplot}},
  \code{\link{co.intervals}}, \code{\link{Lattice}} 
}
\keyword{print}
\keyword{hplot}
\keyword{methods}
\keyword{classes}



