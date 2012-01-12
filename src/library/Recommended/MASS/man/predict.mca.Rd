% file MASS/man/predict.mca.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{predict.mca}
\alias{predict.mca}
\title{
Predict Method for Class 'mca'
}
\description{
Used to compute coordinates for additional rows or additional factors
in a multiple correspondence analysis.
}
\usage{
\method{predict}{mca}(object, newdata, type = c("row", "factor"), \dots)
}
\arguments{
\item{object}{
An object of class \code{"mca"}, usually the result of a call to \code{mca}.
}
\item{newdata}{
A data frame containing \emph{either} additional rows of the factors used to
fit \code{object} \emph{or} additional factors for the cases used in the
original fit.
}
\item{type}{
Are predictions required for further rows or for new factors?
}
\item{\dots}{
Additional arguments from \code{predict}: unused.
}}
\value{
If \code{type = "row"}, the coordinates for the additional rows.


If \code{type = "factor"}, the coordinates of the column vertices for the
levels of the new factors.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{mca}}, \code{\link{plot.mca}}
}
\keyword{category}
\keyword{multivariate}
