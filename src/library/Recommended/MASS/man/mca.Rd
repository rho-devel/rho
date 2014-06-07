% file MASS/man/mca.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{mca}
\alias{mca}
\alias{print.mca}
\title{
Multiple Correspondence Analysis
}
\description{
Computes a multiple correspondence analysis of a set of factors.
}
\usage{
mca(df, nf = 2, abbrev = FALSE)
}
\arguments{
\item{df}{
A data frame containing only factors
}
\item{nf}{
The number of dimensions for the MCA. Rarely 3 might be useful.
}
\item{abbrev}{
Should the vertex names be abbreviated?  By default these are of the
form \sQuote{factor.level} but if \code{abbrev = TRUE} they are just
\sQuote{level} which will suffice if the factors have distinct levels.
}}
\value{
An object of class \code{"mca"}, with components

\item{rs}{
The coordinates of the rows, in \code{nf} dimensions.
}
\item{cs}{
The coordinates of the column vertices, one for each level of each factor.
}
\item{fs}{
Weights for each row, used to interpolate additional factors in \code{predict.mca}.
}
\item{p}{
The number of factors
}
\item{d}{
The singular values for the \code{nf} dimensions.
}
\item{call}{
The matched call.
}}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{predict.mca}}, \code{\link{plot.mca}}, \code{\link{corresp}}
}
\examples{
farms.mca <- mca(farms, abbrev=TRUE)
farms.mca
plot(farms.mca)
}
\keyword{category}
\keyword{multivariate}
