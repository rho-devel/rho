% File nlme/man/varComb.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: varComb.Rd,v 1.5.2.1 2003/04/17 22:28:45 bates Exp $
\name{varComb}
\title{Combination of Variance Functions}
\usage{
varComb(\dots)
}
\alias{varComb}
\arguments{
 \item{\dots}{objects inheriting from class \code{varFunc} representing
   variance function structures.}
}
\description{
  This function is a constructor for the \code{varComb} class,
  representing a combination of variance functions. The corresponding
  variance function is equal to the product of the variance functions of
  the \code{varFunc} objects listed in \code{\dots}.
}
\value{
  a \code{varComb} object representing a combination of variance
  functions, also inheriting from class \code{varFunc}.
}
\references{
 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer.  
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{
  \code{\link{varClasses}},
  \code{\link{varWeights.varComb}},
  \code{\link{coef.varComb}}}

\examples{
vf1 <- varComb(varIdent(form = ~1|Sex), varPower())
}
\keyword{models}
