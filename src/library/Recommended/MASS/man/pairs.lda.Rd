% file MASS/man/pairs.lda.Rd
% copyright (C) 1998-9 W. N. Venables and B. D. Ripley
%
\name{pairs.lda}
\alias{pairs.lda}
\title{
Produce Pairwise Scatterplots from an 'lda' Fit
}
\description{
Pairwise scatterplot of the data on the linear discriminants.
}
\usage{
\method{pairs}{lda}(x, labels = colnames(x), panel = panel.lda,
     dimen, abbrev = FALSE, \dots, cex=0.7, type = c("std", "trellis"))
}
\arguments{
\item{x}{
Object of class \code{"lda"}.
}
\item{labels}{
vector of character strings for labelling the variables.
}
\item{panel}{
panel function to plot the data in each panel.
}
\item{dimen}{
The number of linear discriminants to be used for the plot; if this
exceeds the number determined by \code{x} the smaller value is used.
}
\item{abbrev}{
whether the group labels are abbreviated on the plots. If \code{abbrev > 0}
this gives \code{minlength} in the call to \code{abbreviate}.
}
\item{\dots}{
additional arguments for \code{pairs.default}.
}
\item{cex}{
graphics parameter \code{cex} for labels on plots.
}
\item{type}{
type of plot. The default is in the style of \code{\link{pairs.default}}; the
style \code{"trellis"} uses the Trellis function \code{\link[lattice]{splom}}.
}}
\details{
This function is a method for the generic function
\code{pairs()} for class \code{"lda"}.
It can be invoked by calling \code{pairs(x)} for an
object \code{x} of the appropriate class, or directly by
calling \code{pairs.lda(x)} regardless of the
class of the object.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{pairs}}
}
\keyword{hplot}
\keyword{multivariate}
