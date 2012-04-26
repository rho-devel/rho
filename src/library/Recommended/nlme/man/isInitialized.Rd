% File nlme/man/isInitialized.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: isInitialized.Rd,v 1.4.4.1 2002/08/09 19:45:29 bates Exp $
\name{isInitialized}
\title{Check if Object is Initialized}
\usage{
isInitialized(object)
}
\alias{isInitialized}
\alias{isInitialized.pdMat}
\alias{isInitialized.pdBlocked}
\arguments{
 \item{object}{any object requiring initialization.}
}
\description{
  Checks if \code{object} has been initialized (generally through a call
  to \code{Initialize}), by searching for components and attributes
  which are modified during initialization.
}
\value{
  a logical value indicating whether \code{object} has been
  initialized. 
}
\author{Jose Pinheiro and Douglas Bates }

\seealso{\code{\link{Initialize}}}

\examples{
pd1 <- pdDiag(~age)
isInitialized(pd1)
}
\keyword{models}
