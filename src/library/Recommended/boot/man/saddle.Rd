\name{saddle}
\alias{saddle}
\title{
Saddlepoint Approximations for Bootstrap Statistics
}
\description{
  This function calculates a saddlepoint approximation to the
  distribution of a linear combination of \bold{W} at a particular point
  \code{u}, where \bold{W} is a vector of random variables.  The
  distribution of \bold{W} may be multinomial (default), Poisson or
  binary.  Other distributions are possible also if the adjusted
  cumulant generating function and its second derivative are given.
  Conditional saddlepoint approximations to the distribution of one
  linear combination given the values of other linear combinations of
  \bold{W} can be calculated for \bold{W} having binary or Poisson
  distributions.
}
\usage{
saddle(A = NULL, u = NULL, wdist = "m", type = "simp", d = NULL,
       d1 = 1, init = rep(0.1, d), mu = rep(0.5, n), LR = FALSE,
       strata = NULL, K.adj = NULL, K2 = NULL)
}
\arguments{
  \item{A}{
    A vector or matrix of known coefficients of the linear combinations
    of \bold{W}. It is a required argument unless \code{K.adj} and
    \code{K2} are supplied, in which case it is ignored.
  }
  \item{u}{
    The value at which it is desired to calculate the saddlepoint
    approximation to the distribution of the linear combination of
    \bold{W}. It is a required argument unless \code{K.adj} and
    \code{K2} are supplied, in which case it is ignored.
  }
  \item{wdist}{
    The distribution of \bold{W}.  This can be one of \code{"m"}
    (multinomial), \code{"p"} (Poisson), \code{"b"} (binary) or
    \code{"o"} (other).  If \code{K.adj} and \code{K2} are given
    \code{wdist} is set to \code{"o"}.
  }
  \item{type}{
    The type of saddlepoint approximation.  Possible types are
    \code{"simp"} for simple saddlepoint and \code{"cond"} for the
    conditional saddlepoint.  When \code{wdist} is \code{"o"} or
    \code{"m"}, \code{type} is automatically set to \code{"simp"}, which
    is the only type of saddlepoint currently implemented for those
    distributions.
  }
  \item{d}{
    This specifies the dimension of the whole statistic.  This argument
    is required only when \code{wdist = "o"} and defaults to 1 if not
    supplied in that case.  For other distributions it is set to
    \code{ncol(A)}.
  }
  \item{d1}{
    When \code{type} is \code{"cond"} this is the dimension of the
    statistic of interest which must be less than \code{length(u)}.
    Then the saddlepoint approximation to the conditional distribution
    of the first \code{d1} linear combinations given the values of the
    remaining combinations is found.  Conditional distribution function
    approximations can only be found if the value of \code{d1} is 1.
  }
  \item{init}{
    Used if \code{wdist} is either \code{"m"} or \code{"o"}, this gives
    initial values to \code{nlmin} which is used to solve the
    saddlepoint equation.
  }
  \item{mu}{
    The values of the parameters of the distribution of \bold{W} when
    \code{wdist} is \code{"m"}, \code{"p"} \code{"b"}.  \code{mu} must
    be of the same length as W (i.e. \code{nrow(A)}). The default is
    that all values of \code{mu} are equal and so the elements of
    \bold{W} are identically distributed.
  }
  \item{LR}{
    If \code{TRUE} then the Lugananni-Rice approximation to the cdf is used,
    otherwise the approximation used is based on Barndorff-Nielsen's r*. 
  }
  \item{strata}{
    The strata for stratified data.
  }
  \item{K.adj}{
    The adjusted cumulant generating function used when \code{wdist} is
    \code{"o"}.  This is a function of a single parameter, \code{zeta},
    which calculates \code{K(zeta)-u\%*\%zeta}, where \code{K(zeta)} is
    the cumulant generating function of \bold{W}.
  }
  \item{K2}{
    This is a function of a single parameter \code{zeta} which returns the 
    matrix of second derivatives of \code{K(zeta)} for use when
    \code{wdist} is \code{"o"}.  If \code{K.adj} is given then this must
    be given also.  It is called only once with the calculated solution
    to the saddlepoint equation being passed as the argument.  This
    argument is ignored if \code{K.adj} is not supplied.
  }
}
\value{
A list consisting of the following components

\item{spa}{
  The saddlepoint approximations.  The first value is the density approximation
  and the second value is the distribution function approximation.
}
\item{zeta.hat}{
  The solution to the saddlepoint equation.  For the conditional saddlepoint
  this is the solution to the saddlepoint equation for the numerator.
}
\item{zeta2.hat}{
  If \code{type} is \code{"cond"} this is the solution to the
  saddlepoint equation for the denominator.  This component is not
  returned for any other value of \code{type}.
}
}
\details{
  If \code{wdist} is \code{"o"} or \code{"m"}, the saddlepoint equations
  are solved using \code{nlmin} to minimize \code{K.adj} with respect to
  its parameter \code{zeta}.  For the Poisson and binary cases, a
  generalized linear model is fitted such that the parameter estimates
  solve the saddlepoint equations.  The response variable 'y' for the
  \code{glm} must satisfy the equation \code{t(A)\%*\%y = u} (\code{t()}
  being the transpose function).  Such a vector can be found as a feasible
  solution to a linear programming problem.  This is done by a call to
  \code{simplex}.  The covariate matrix for the \code{glm} is given by
  \code{A}.
}
\references{
Booth, J.G. and Butler, R.W. (1990) Randomization distributions and 
saddlepoint approximations in generalized linear models. 
\emph{Biometrika}, \bold{77}, 787--796.


Canty, A.J. and Davison, A.C. (1997) Implementation of saddlepoint 
approximations to resampling distributions.  
\emph{Computing Science and Statistics; Proceedings of the 28th Symposium on the Interface}, 248--253.


Davison, A.C. and Hinkley, D.V. (1997) 
\emph{Bootstrap Methods and their Application}. Cambridge University Press.


Jensen, J.L. (1995) \emph{Saddlepoint Approximations}. Oxford University Press.
}
\seealso{
  \code{\link{saddle.distn}}, \code{\link{simplex}}
}
\examples{
# To evaluate the bootstrap distribution of the mean failure time of 
# air-conditioning equipment at 80 hours
saddle(A = aircondit$hours/12, u = 80)

# Alternatively this can be done using a conditional poisson
saddle(A = cbind(aircondit$hours/12,1), u = c(80, 12),
       wdist = "p", type = "cond")

# To use the Lugananni-Rice approximation to this
saddle(A = cbind(aircondit$hours/12,1), u = c(80, 12),
       wdist = "p", type = "cond", 
       LR = TRUE)

# Example 9.16 of Davison and Hinkley (1997) calculates saddlepoint 
# approximations to the distribution of the ratio statistic for the
# city data. Since the statistic is not in itself a linear combination
# of random Variables, its distribution cannot be found directly.  
# Instead the statistic is expressed as the solution to a linear 
# estimating equation and hence its distribution can be found.  We
# get the saddlepoint approximation to the pdf and cdf evaluated at
# t = 1.25 as follows.
jacobian <- function(dat,t,zeta)
{
     p <- exp(zeta*(dat$x-t*dat$u))
     abs(sum(dat$u*p)/sum(p))
}
city.sp1 <- saddle(A = city$x-1.25*city$u, u = 0)
city.sp1$spa[1] <- jacobian(city, 1.25, city.sp1$zeta.hat) * city.sp1$spa[1]
city.sp1
}
\keyword{smooth}
\keyword{nonparametric}
% Converted by Sd2Rd version 1.15.
