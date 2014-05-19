% File nlme/man/summary.lmList.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: summary.lmList.Rd,v 1.7.2.1 2003/05/17 22:06:46 bates Exp $
\name{summary.lmList}
\title{Summarize an lmList Object}
\usage{
\method{summary}{lmList}(object, pool, \dots)
}
\alias{summary.lmList}
\arguments{
 \item{object}{an object inheriting from class \code{lmList}, representing
   a list of \code{lm} fitted objects.
 }
 \item{pool}{an optional logical value indicating whether a pooled
   estimate of the residual standard error should be used. Default is
   \code{attr(object, "pool")}.
 }
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  The \code{summary.lm} method is applied to each \code{lm} component of
  \code{object} to produce summary information on the individual fits,
  which is organized into a list of summary statistics. The returned
  object is suitable for printing with the \code{print.summary.lmList}
  method. 
}
\value{
  a list with summary statistics obtained by applying \code{summary.lm}
  to the elements of \code{object}, inheriting from class
  \code{summary.lmList}. The components of \code{value} are:
  \item{call}{a list containing an image of the \code{lmList} call that
    produced \code{object}.
  }
  \item{coefficients}{a three dimensional array with summary information
    on the \code{lm} coefficients. The first dimension corresponds to
    the names of the \code{object} components, the second dimension is
    given by   \code{"Value"}, \code{"Std. Error"}, \code{"t value"},
    and \code{"Pr(>|t|)"}, corresponding, respectively, to the
    coefficient estimates and their associated standard errors,
    t-values, and p-values. The third dimension is given by the
    coefficients names.
  }
  \item{correlation}{a three dimensional array with the 
    correlations between the individual \code{lm} coefficient
    estimates. The first dimension corresponds to the names of the
    \code{object} components. The third dimension is given by the
    coefficients names. For each coefficient, the rows of the associated
    array give the correlations between that coefficient and the
    remaining coefficients, by \code{lm} component.
  }
  \item{cov.unscaled}{a three dimensional array with the unscaled
    variances/covariances for the individual \code{lm} coefficient
    estimates (giving the estimated variance/covariance for the
    coefficients, when multiplied by the estimated residual errors). The
    first dimension corresponds to the names of the \code{object}
    components. The third dimension is given by the
    coefficients names. For each coefficient, the rows of the associated
    array give the unscaled covariances between that coefficient and the
    remaining coefficients, by \code{lm} component.
  }
  \item{df}{an array with the number of degrees of freedom for the model
    and for residuals, for each \code{lm} component.
  }
  \item{df.residual}{the total number of degrees of freedom for
    residuals, corresponding to the sum of residuals df of all \code{lm}
    components.
  }
  \item{fstatistics}{an array with the F test statistics and
    corresponding degrees of freedom, for each \code{lm} component.
  }
  \item{pool}{the value of the \code{pool} argument to the function.
  }
  \item{r.squared}{a vector with the multiple R-squared statistics for
    each \code{lm} component.
  }
  \item{residuals}{a list with components given by the residuals from
    individual \code{lm} fits.
  }
  \item{RSE}{the pooled estimate of the residual standard error.}
  \item{sigma}{a vector with the residual standard error estimates for
    the individual \code{lm} fits.
  }
  \item{terms}{the terms object used in fitting the individual \code{lm}
    components.
  }
}

\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{lmList}},
  \code{\link{summary}}
}

\examples{
fm1 <- lmList(distance ~ age | Subject, Orthodont)
summary(fm1)
}
\keyword{models}
