% File nlme/man/solve.reStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: solve.reStruct.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{solve.reStruct}
\title{Apply Solve to an reStruct Object}
\usage{
\method{solve}{reStruct}(a, b, \dots)
}
\alias{solve.reStruct}
\arguments{
 \item{a}{an object inheriting from class \code{reStruct},
   representing a random effects structure and consisting of a list of
   \code{pdMat} objects.} 
 \item{b}{this argument is only included for consistency with the
   generic function and is not used in this method function.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  \code{Solve} is applied to each \code{pdMat} component of \code{a},
  which results in inverting the positive-definite matrices they
  represent. 
}
\value{
  an \code{reStruct} object similar to \code{a}, but with the
  \code{pdMat} components representing the inverses of the
  matrices represented by the components of \code{a}.
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{solve.pdMat}}, \code{\link{reStruct}}}

\examples{
rs1 <- reStruct(list(A = pdSymm(diag(1:3), form = ~Score),
  B = pdDiag(2 * diag(4), form = ~Educ)))
solve(rs1)
}
\keyword{models}
