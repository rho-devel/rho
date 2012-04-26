% File nlme/man/lmeControl.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: lmeControl.Rd,v 1.8 2002/03/05 14:59:39 bates Exp $
\name{lmeControl}
\title{Control Values for lme Fit}
\usage{
lmeControl(maxIter, msMaxIter, tolerance, niterEM, msMaxEval, msTol,
           msScale, msVerbose, returnObject, gradHess, apVar,
           .relStep, minAbsParApVar, nlmStepMax,
           opt = c("nlminb", "optim"), optimMethod,
           natural)
}
\alias{lmeControl}
\arguments{
 \item{maxIter}{maximum number of iterations for the \code{lme}
   optimization algorithm. Default is 50.} 
 \item{msMaxIter}{maximum number of iterations
   for the \code{nlm} optimization step inside the \code{lme}
   optimization. Default is 50.}
 \item{tolerance}{tolerance for the convergence criterion in the
   \code{lme} algorithm. Default is 1e-6.}
 \item{niterEM}{number of iterations for the EM algorithm used to refine
   the initial estimates of the random effects variance-covariance
   coefficients. Default is 25.}
 \item{msMaxEval}{maximum number of evaluations of the objective
   function permitted for nlminb. Default is 200. }
 \item{msTol}{tolerance for the convergence criterion in \code{nlm},
   passed as the \code{rel.tolerance} argument to the function (see
   documentation on \code{nlm}). Default is 1e-7. }
 \item{msScale}{scale function passed as the \code{scale} argument to
   the \code{nlm} function (see documentation on that function). Default
   is \code{lmeScale}.}
 \item{msVerbose}{a logical value passed as the \code{trace} argument to
   \code{nlm} (see documentation on that function). Default is
   \code{FALSE}.} 
 \item{returnObject}{a logical value indicating whether the fitted
   object should be returned when the maximum number of iterations is
   reached without convergence of the algorithm. Default is
   \code{FALSE}.} 
 \item{gradHess}{a logical value indicating whether numerical gradient
   vectors and Hessian matrices of the log-likelihood function should
   be used in the \code{nlm} optimization. This option is only available
   when the correlation structure (\code{corStruct}) and the variance
   function structure (\code{varFunc}) have no "varying" parameters and
   the \code{pdMat} classes used in the random effects structure are
   \code{pdSymm} (general positive-definite), \code{pdDiag} (diagonal),
   \code{pdIdent} (multiple of the identity),  or
   \code{pdCompSymm} (compound symmetry). Default is \code{TRUE}.}
 \item{apVar}{a logical value indicating whether the approximate
   covariance matrix of the variance-covariance parameters should be
   calculated. Default is \code{TRUE}.}
 \item{.relStep}{relative step for numerical derivatives
   calculations. Default is \code{.Machine$double.eps^(1/3)}.}
 \item{nlmStepMax}{stepmax value to be passed to nlm. See
     \code{\link{nlm}} for details. Default is 100.0}
 \item{opt}{the optimizer to be used, either \code{\link{nlminb}} (the
   default since (\R 2.2.0) or \code{\link{optim}} (the previous
   default).}
 \item{optimMethod}{character - the optimization method to be used with
   the \code{\link{optim}} optimizer. The default is
   \code{"BFGS"}.  An alternative is \code{"L-BFGS-B"}.}
 \item{minAbsParApVar}{numeric value - minimum absolute parameter value
   in the approximate variance calculation.  The default is \code{0.05}.}
 \item{natural}{a logical value indicating whether the \code{pdNatural}
   parametrization should be used for general positive-definite matrices
   (\code{pdSymm}) in \code{reStruct}, when the approximate covariance
   matrix of the estimators is calculated. Default is \code{TRUE}.}
}
\description{
  The values supplied in the function call replace the defaults and a
  list with all possible arguments is returned. The returned list is
  used as the \code{control} argument to the \code{lme} function.
}
\value{
  a list with components for each of the possible arguments.
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{lme}}, \code{\link{nlm}}, \code{\link{optim}},
  \code{\link{lmeScale}}}
\examples{
# decrease the maximum number iterations in the ms call and
# request that information on the evolution of the ms iterations be printed
lmeControl(msMaxIter = 20, msVerbose = TRUE)
}
\keyword{models}
