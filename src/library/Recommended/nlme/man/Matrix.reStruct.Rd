% File nlme/man/Matrix.reStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: Matrix.reStruct.Rd,v 1.2.2.1 2003/08/09 22:45:17 bates Exp $
\name{Matrix.reStruct}
\title{Assign reStruct Matrices}
\usage{
\method{matrix}{reStruct}(object) <-  value
}
\alias{matrix<-.reStruct}
\arguments{
 \item{object}{an object inheriting from class \code{reStruct},
   representing a random effects structure and consisting of a list of
   \code{pdMat} objects.} 
 \item{value}{a matrix, or list of matrices, with the new values to be
   assigned to the matrices associated with the \code{pdMat} components
   of \code{object}.}  
}
\description{
  The individual matrices in \code{value} are assigned to each
  \code{pdMat} component of \code{object}, in the order they are listed. The
  new matrices must have the same dimensions as the matrices they are
  meant to replace.
}
\value{
  an \code{reStruct} object similar to \code{object}, but with the
  coefficients of the individual \code{pdMat} components modified to
  produce the matrices listed in \code{value}.
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{reStruct}}, \code{\link{pdMat}}, \code{"\link{matrix<-}"}
}

\examples{
rs1 <- reStruct(list(Dog = ~day, Side = ~1), data = Pixel)
matrix(rs1) <- list(diag(2), 3)
}
\keyword{models}
