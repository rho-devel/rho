\name{formXtViX}
\alias{formXtViX}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Form component of GAMM covariance matrix}
\description{ This is a service routine for \code{\link{gamm}}. Given,
\eqn{V}{V}, an estimated covariance matrix obtained using \code{\link{extract.lme.cov2}} this
routine forms a matrix square root of \eqn{ X^TV^{-1}X}{X'inv(V)X} as efficiently as possible, given
the structure of \eqn{V}{V} (usually sparse).
}
\usage{
formXtViX(V,X)
}

%- maybe also `usage' for other objects documented here.
\arguments{
\item{V}{ A data covariance matrix list returned from \code{\link{extract.lme.cov2}}}

\item{X}{ A model matrix.}

}

\details{ The covariance matrix returned by \code{\link{extract.lme.cov2}} may
be in a packed and re-ordered format, since it is usually sparse. Hence a
special service routine is required to form the required products involving
this matrix. 
}



\value{ A matrix, R such that \code{crossprod(R)} gives \eqn{ X^TV^{-1}X}{X'inv(V)X}.
}

\references{

For \code{lme} see:

Pinheiro J.C. and Bates, D.M. (2000) Mixed effects Models in S and S-PLUS. Springer

For details of how GAMMs are set up for estimation using \code{lme} see:
 
Wood, S.N. (2006) Low rank scale invariant tensor product smooths for
Generalized Additive Mixed Models. Biometrics 62(4):1025-1036

\url{http://www.maths.bath.ac.uk/~sw283/}
}
\author{ Simon N. Wood \email{simon.wood@r-project.org}}


\seealso{  
\code{\link{gamm}}, \code{\link{extract.lme.cov2}} 
}

\examples{
require(mgcv)
library(nlme)
data(ergoStool)
b <- lme(effort ~ Type, data=ergoStool, random=~1|Subject)
V1 <- extract.lme.cov(b, ergoStool)
V2 <- extract.lme.cov2(b, ergoStool)
X <- model.matrix(b, data=ergoStool)
crossprod(formXtViX(V2, X))
t(X)%*%solve(V1,X)
}
\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..





