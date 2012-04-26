% File nlme/man/Names.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: Names.Rd,v 1.5.2.1 2003/05/16 21:49:21 bates Exp $
\name{Names}
\title{Names Associated with an Object}
\usage{
Names(object, \dots)
Names(object, \dots) <- value
}
\alias{Names}
\alias{Names<-}
\arguments{
 \item{object}{any object for which names can be extracted and/or assigned.}
 \item{\dots}{some methods for this generic function require additional
   arguments.}
 \item{value}{names to be assigned to \code{object}.}
}
\description{
  This function is generic; method functions can be written to handle
  specific classes of objects. Classes which already have methods for
  this function include: \code{formula}, \code{modelStruct},
  \code{pdBlocked}, \code{pdMat}, and \code{reStruct}.
}
\value{
  will depend on the method function used; see the appropriate documentation.
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\section{SIDE EFFECTS}{
  On the left side of an assignment, sets the names associated with
  \code{object} to \code{value}, which must have an appropriate length. 
}
\note{
  If \code{names} were generic, there would be no need for this generic
  function. 
}
\seealso{\code{\link{Names.formula}},
  \code{\link{Names.pdMat}}}
\examples{
## see the method function documentation
}
\keyword{models}
