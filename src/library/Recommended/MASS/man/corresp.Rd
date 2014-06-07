% file MASS/man/corresp.Rd
% copyright (C) 1994-2003 W. N. Venables and B. D. Ripley
%
\name{corresp}
\alias{corresp}
\alias{corresp.xtabs}
\alias{corresp.data.frame}
\alias{corresp.default}
\alias{corresp.factor}
\alias{corresp.formula}
\alias{corresp.matrix}
% \alias{biplot.correspondence}
% \alias{plot.correspondence}
% \alias{print.correspondence}
\title{
Simple Correspondence Analysis
}
\description{
Find the principal canonical correlation and corresponding row- and
column-scores from a correspondence analysis of a two-way contingency
table.
}
\usage{
corresp(x, \dots)

\method{corresp}{matrix}(x, nf = 1, \dots)

\method{corresp}{factor}(x, y, \dots)

\method{corresp}{data.frame}(x, \dots)

\method{corresp}{xtabs}(x, \dots)

\method{corresp}{formula}(formula, data, \dots)
}
\arguments{
  \item{x, formula}{
    The function is generic, accepting various forms of the principal
    argument for specifying a two-way frequency table.  Currently accepted
    forms are matrices, data frames (coerced to frequency tables), objects
    of class \code{"\link{xtabs}"} and formulae of the form \code{~ F1 + F2},
    where \code{F1} and \code{F2} are factors.
  }
  \item{nf}{
    The number of factors to be computed. Note that although 1 is the most
    usual, one school of thought takes the first two singular vectors for
    a sort of biplot.
  }
  \item{y}{a second factor for a cross-classification.}
  \item{data}{a data frame against which to preferentially resolve
    variables in the formula.}
  \item{\dots}{
    If the principal argument is a formula, a data frame may be specified
    as well from which variables in the formula are preferentially
    satisfied.
  }
}
\value{
  An list object of class \code{"correspondence"} for which
  \code{print}, \code{plot} and \code{biplot} methods are supplied.
  The main components are the canonical correlation(s) and the row
  and column scores.
}
\details{
  See Venables & Ripley (2002).  The \code{plot} method produces a graphical
  representation of the table if \code{nf=1}, with the \emph{areas} of circles
  representing the numbers of points.  If \code{nf} is two or more the
  \code{biplot} method is called, which plots the second and third columns of
  the matrices \code{A = Dr^(-1/2) U L} and \code{B = Dc^(-1/2) V L} where the
  singular value decomposition is \code{U L V}.  Thus the x-axis is the
  canonical correlation times the row and column scores.  Although this
  is called a biplot, it does \emph{not} have any useful inner product
  relationship between the row and column scores.  Think of this as an
  equally-scaled plot with two unrelated sets of labels.  The origin is
  marked on the plot with a cross.  (For other versions of this plot see
  the book.)
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.

  Gower, J. C. and Hand, D. J. (1996)
  \emph{Biplots.}  Chapman & Hall.
}
\seealso{
  \code{\link{svd}}, \code{\link{princomp}}.
}
\examples{
(ct <- corresp(~ Age + Eth, data = quine))
\dontrun{plot(ct)}

corresp(caith)
biplot(corresp(caith, nf = 2))
}
\keyword{category}
\keyword{multivariate}
