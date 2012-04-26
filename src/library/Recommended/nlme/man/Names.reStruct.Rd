% File nlme/man/Names.reStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: Names.reStruct.Rd,v 1.8.2.1 2003/08/09 22:45:17 bates Exp $
\name{Names.reStruct}
\title{Names of an reStruct Object}
\usage{
\method{Names}{reStruct}(object, \dots)
\method{Names}{reStruct}(object, \dots) <- value
}
\alias{Names.reStruct}
\alias{Names<-.reStruct}
\arguments{
 \item{object}{an object inheriting from class \code{reStruct},
   representing a random effects structure and consisting of a list of
   \code{pdMat} objects.} 
 \item{value}{a list of character vectors with the replacement values
   for the names of the individual \code{pdMat} objects that form
   \code{object}. It must have the same length as \code{object}.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function extracts the column names of each of the
  positive-definite matrices represented the \code{pdMat}
  elements of \code{object}.
}
\value{
  a list containing the column names of each of the positive-definite
  matrices represented by the \code{pdMat} elements of \code{object}. 
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\section{SIDE EFFECTS}{
  On the left side of an assignment, sets the \code{Names} of the
  \code{pdMat} elements of \code{object} to the corresponding element of
  \code{value}.  
}
\seealso{\code{\link{reStruct}}, \code{\link{pdMat}},
  \code{\link{Names.pdMat}}
} 
\examples{
rs1 <- reStruct(list(Dog = ~day, Side = ~1), data = Pixel)
Names(rs1)
}
\keyword{models}
