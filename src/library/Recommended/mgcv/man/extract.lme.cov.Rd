\name{extract.lme.cov}
\alias{extract.lme.cov}
\alias{extract.lme.cov2}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Extract the data covariance matrix from an lme object}
\description{ This is a service routine for \code{\link{gamm}}. Extracts 
the estimated covariance matrix of the data from an \code{lme} object, allowing the 
user control about which levels of random effects to include in this 
calculation. \code{extract.lme.cov} forms the full matrix explicitly:
\code{extract.lme.cov2} tries to be more economical than this.
}
\usage{
extract.lme.cov(b,data,start.level=1)
extract.lme.cov2(b,data,start.level=1)
}

%- maybe also `usage' for other objects documented here.
\arguments{
\item{b}{ A fitted model object returned by a call to \code{\link[nlme]{lme}}}.

\item{data}{ The data frame/ model frame that was supplied to
 \code{\link[nlme]{lme}}.}

\item{start.level}{The level of nesting at which to start including random 
effects in the calculation. This is used to allow smooth terms to be estimated
as random effects, but treated like fixed effects for variance calculations.}

}

\details{ The random effects, correlation structure and variance structure used
for a linear mixed model combine to imply a covariance matrix for the 
response data being modelled. These routines extracts that covariance matrix.
The process is slightly complicated, because different components of the 
fitted model object are stored in different orders (see function code for 
details!).  

The \code{extract.lme.cov} calculation is not optimally efficient, since it forms the full matrix,
which may in fact be sparse. \code{extract.lme.cov2} is more efficient. If the
covariance matrix is diagonal, then only the leading diagonal is returned; if
it can be written as a block diagonal matrix (under some permutation of the
original data) then a list of matrices defining the non-zero blocks is
returned along with an index indicating which row of the original data each
row/column of the block diagonal matrix relates to. The block sizes are defined by
the coarsest level of grouping in the random effect structure.

\code{\link{gamm}} uses \code{extract.lme.cov2}.

\code{extract.lme.cov} does not currently deal with the situation in which the
grouping factors for a correlation structure are finer than those for the
random effects. \code{extract.lme.cov2} does deal with this situation.

}



\value{ For \code{extract.lme.cov} an estimated covariance matrix.

For \code{extract.lme.cov2} a list containing the estimated covariance matrix
and an indexing array. The covariance matrix is stored as the elements on the
leading diagonal, a list of the matrices defining a block diagonal matrix, or
a full matrix if the previous two options are not possible.
}

\references{

For \code{lme} see:

Pinheiro J.C. and Bates, D.M. (2000) Mixed effects Models in S and S-PLUS. Springer

For details of how GAMMs are set up here for estimation using \code{lme} see:
 
Wood, S.N. (2006) Low rank scale invariant tensor product smooths for
Generalized Additive Mixed Models. Biometrics 62(4):1025-1036

or 

Wood S.N. (2006) Generalized Additive Models: An Introduction with R. Chapman
and Hall/CRC Press.

\url{http://www.maths.bath.ac.uk/~sw283/}
}
\author{ Simon N. Wood \email{simon.wood@r-project.org}}


\seealso{  
\code{\link{gamm}}, \code{\link{formXtViX}} 
}

\examples{
## see also ?formXtViX for use of extract.lme.cov2
require(mgcv)
library(nlme)
data(Rail)
b <- lme(travel~1,Rail,~1|Rail)
extract.lme.cov(b,Rail)
}
\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..





