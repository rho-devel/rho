% File nlme/man/intervals.lmList.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: intervals.lmList.Rd,v 1.8 2002/03/05 14:59:39 bates Exp $
\name{intervals.lmList}
\title{Confidence Intervals on lmList Coefficients}
\usage{
\method{intervals}{lmList}(object, level, pool, \dots)
}
\alias{intervals.lmList}
\alias{print.intervals.lmList}
\arguments{
 \item{object}{an object inheriting from class \code{lmList}, representing
   a list of \code{lm} objects with a common model.
 }
 \item{level}{an optional numeric value with the confidence level for
   the intervals. Defaults to 0.95.
 }
 \item{pool}{an optional logical value indicating whether a pooled
   estimate of the residual standard error should be used. Default is
   \code{attr(object, "pool")}.
 }
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  Confidence intervals on the linear model coefficients are obtained for
  each \code{lm} component of \code{object} and organized into a three
  dimensional array. The first dimension corresponding to the names
  of the \code{object} components. The second dimension is given by
  \code{lower}, \code{est.}, and \code{upper} corresponding,
  respectively, to the lower confidence limit, estimated coefficient,
  and upper confidence limit. The third dimension is given by the
  coefficients names. 
}
\value{
  a three dimensional array with the confidence intervals and estimates
  for the coefficients of each \code{lm} component of \code{object}. 
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\references{
  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer.  
}
\seealso{\code{\link{lmList}}, \code{\link{intervals}},
  \code{\link{plot.intervals.lmList}}
  }

\examples{
fm1 <- lmList(distance ~ age | Subject, Orthodont)
intervals(fm1)
}
\keyword{models}
