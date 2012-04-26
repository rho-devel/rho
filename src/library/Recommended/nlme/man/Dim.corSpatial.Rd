% File nlme/man/Dim.corSpatial.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: Dim.corSpatial.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{Dim.corSpatial}
\title{Dimensions of a corSpatial Object}
\usage{
\method{Dim}{corSpatial}(object, groups, \dots)
}
\alias{Dim.corSpatial}
\arguments{
 \item{object}{an object inheriting from class \code{corSpatial},
   representing a spatial correlation structure.}
 \item{groups}{an optional factor defining the grouping of the
   observations; observations within a group are correlated and
   observations in different groups are uncorrelated.}
 \item{\dots}{further arguments to be passed to or from methods.}
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
  \item{spClass}{an integer representing the spatial correlation class;
    0 = user defined class, 1 = \code{corSpher}, 2 = \code{corExp}, 3 =
    \code{corGaus}, 4 = \code{corLin}}
  \item{sumLenSq}{sum of the squares of the number of observations per
    group} 
  \item{len}{an integer vector with the number of observations per
    group}
  \item{start}{an integer vector with the starting position for the
    distance vectors in each group, beginning from zero}
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{Dim}},
  \code{\link{Dim.corStruct}}

}

\examples{
Dim(corGaus(), getGroups(Orthodont))

cs1ARMA <- corARMA(0.4, form = ~ 1 | Subject, q = 1)
cs1ARMA <- Initialize(cs1ARMA, data = Orthodont)
Dim(cs1ARMA)
}
\keyword{models}
