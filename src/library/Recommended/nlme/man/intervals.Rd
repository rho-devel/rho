% File nlme/man/intervals.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: intervals.Rd,v 1.4.2.1 2003/04/17 22:28:45 bates Exp $
\name{intervals}
\title{Confidence Intervals on Coefficients}
\usage{
intervals(object, level, \dots)
}
\alias{intervals}
\arguments{
 \item{object}{a fitted model object from which parameter estimates can
   be extracted.}
 \item{level}{an optional numeric value for the interval confidence
   level. Defaults to 0.95.}
 \item{\dots}{some methods for the generic may require additional
   arguments.}
}
\description{
  Confidence intervals on the parameters associated with the model
  represented by \code{object} are obtained. This function is generic;
  method functions can be written to handle specific classes of
  objects. Classes which already have methods for this function include:
  \code{gls}, \code{lme}, and \code{lmList}. 
}
\value{
  will depend on the method function used; see the appropriate documentation.
}
\references{
  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer.  
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{intervals.lme}},
  \code{\link{intervals.lmList}},
  \code{\link{intervals.gls}}
}
\examples{
## see the method documentation
}
\keyword{models}
