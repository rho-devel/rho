% file MASS/man/plot.lda.Rd
% copyright (C) 1998-9 W. N. Venables and B. D. Ripley
%
\name{plot.lda}
\alias{plot.lda}
\title{
Plot Method for Class 'lda'
}
\description{
Plots a set of data on one, two or more linear discriminants.
}
\usage{
\method{plot}{lda}(x, panel = panel.lda, \dots, cex = 0.7, dimen,
     abbrev = FALSE, xlab = "LD1", ylab = "LD2")
}
\arguments{
\item{x}{
An object of class \code{"lda"}.
}
\item{panel}{
the panel function used to plot the data.
}
\item{\dots}{
additional arguments to \code{pairs}, \code{ldahist} or \code{eqscplot}.
}
\item{cex}{
graphics parameter \code{cex} for labels on plots.
}
\item{dimen}{
The number of linear discriminants to be used for the plot; if this
exceeds the number determined by \code{x} the smaller value is used.
}
\item{abbrev}{
whether the group labels are abbreviated on the plots. If \code{abbrev > 0}
this gives \code{minlength} in the call to \code{abbreviate}.
}
\item{xlab}{
label for the x axis
}
\item{ylab}{
label for the y axis
}}
\details{
This function is a method for the generic function
\code{plot()} for class \code{"lda"}.
It can be invoked by calling \code{plot(x)} for an
object \code{x} of the appropriate class, or directly by
calling \code{plot.lda(x)} regardless of the
class of the object.

The behaviour is determined by the value of \code{dimen}. For
\code{dimen > 2}, a \code{pairs} plot is used. For \code{dimen = 2}, an
equiscaled scatter plot is drawn. For \code{dimen = 1}, a set of
histograms or density plots are drawn.  Use argument \code{type} to
match \code{"histogram"} or \code{"density"} or \code{"both"}.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{pairs.lda}}, \code{\link{ldahist}}, \code{\link{lda}}, \code{\link{predict.lda}}
}
\keyword{hplot}
\keyword{multivariate}
