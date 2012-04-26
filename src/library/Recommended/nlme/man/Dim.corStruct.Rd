% File nlme/man/Dim.corStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: Dim.corStruct.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{Dim.corStruct}
\title{Dimensions of a corStruct Object}
\usage{
\method{Dim}{corStruct}(object, groups, \dots)
}
\alias{Dim.corStruct}
\arguments{
 \item{object}{an object inheriting from class \code{corStruct},
   representing a correlation structure.}
 \item{groups}{an optional factor defining the grouping of the
   observations; observations within a group are correlated and
   observations in different groups are uncorrelated.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  if \code{groups} is missing, it returns the \code{Dim} attribute of
  \code{object}; otherwise, calculates the dimensions associated with
  the grouping factor.
}
\value{
  a list with components:
  \item{N}{length of \code{groups}}
  \item{M}{number of groups}
  \item{maxLen}{maximum number of observations in a group}
  \item{sumLenSq}{sum of the squares of the number of observations per
    group} 
  \item{len}{an integer vector with the number of observations per
    group}
  \item{start}{an integer vector with the starting position for the
    observations in each group, beginning from zero}
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{Dim}}, \code{\link{Dim.corSpatial}}}

\examples{
Dim(corAR1(), getGroups(Orthodont))
}
\keyword{models}
