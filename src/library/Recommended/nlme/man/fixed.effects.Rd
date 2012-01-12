% File nlme/man/fixed.effects.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: fixed.effects.Rd,v 1.3.4.2 2003/05/17 21:57:49 bates Exp $
\name{fixed.effects}
\title{Extract Fixed Effects}
\usage{
fixed.effects(object, \dots)
fixef(object, \dots)
}
\alias{fixed.effects}
\alias{fixef}
\arguments{
 \item{object}{any fitted model object from which fixed effects
   estimates can be extracted.}
 \item{\dots}{some methods for this generic function require additional
   arguments.}
}
\description{
  This function is generic; method functions can be written to handle
  specific classes of objects. Classes which already have methods for
  this function include \code{lmList} and \code{lme}.
}
\value{
  will depend on the method function used; see the appropriate documentation.
}
\references{
  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer.  
}
\seealso{\code{\link{fixef.lmList}}} 
\examples{
## see the method function documentation
}
\keyword{models}
