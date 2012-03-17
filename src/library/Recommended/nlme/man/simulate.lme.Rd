% File nlme/man/simulate.lme.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{simulate.lme}
\title{Simulate Results from \code{lme} Models}
\usage{
\method{simulate}{lme}(object, nsim, seed, m2, method, niterEM, useGen, \dots)
}
\alias{simulate.lme}
\alias{plot.simulate.lme}
\alias{print.simulate.lme}
\arguments{
  \item{object}{
    an object inheriting from class \code{lme}, representing a fitted
    linear mixed-effects model, or a list containing an lme model
    specification.  If given as a list, it should contain
    components \code{fixed}, \code{data}, and \code{random}
    with values suitable for a call to \code{\link{lme}}. This argument
    defines the null model.
  }
  \item{m2}{
    an \code{lme} object, or a list, like \code{m1} containing a second
    lme model specification. This argument defines the alternative model.
    If given as a list, only those parts of the specification that
    change between model \code{m1} and \code{m2} need to be specified.
  }
  \item{seed}{
    an optional integer that is passed to \code{set.seed}.  Defaults to
    a random integer.
  }
  \item{method}{
   an optional character array.  If it includes \code{"REML"} the models
   are fit by maximizing the restricted log-likelihood. If it includes
   \code{"ML"} the log-likelihood is maximized.  Defaults to
   \code{c("REML", "ML")}, in which case both methods are used.
  }
  \item{nsim}{
    an optional positive integer specifying the number of simulations to
    perform.  Defaults to \bold{1}. \bold{This has changed.  Previously
      the default was 1000.}
  }
  \item{niterEM}{
    an optional integer vector of length 2 giving the number of
    iterations of the EM algorithm to apply when fitting the \code{m1}
    and \code{m2} to each simulated set of data. Defaults to
    \code{c(40,200)}. 
  }
  \item{useGen}{
    an optional logical value. If \code{TRUE}, numerical derivatives are
    used to obtain the gradient and the Hessian of the log-likelihood in
    the optimization algorithm in the \code{ms} function. If
    \code{FALSE}, the default algorithm in \code{ms} for functions that
    do not incorporate gradient and Hessian attributes is used. Default
    depends on the \code{pdMat} classes used in \code{m1} and \code{m2}:
    if both are standard classes (see \code{\link{pdClasses}}) then
    defaults to \code{TRUE}, otherwise defaults to \code{FALSE}.
  }
  \item{\dots}{optional additional arguments.  None are used.}
}
\description{
  The model \code{object} is fit to the data.  Using
  the fitted values of the parameters, \code{nsim} new data vectors from
  this model are simulated.  Both \code{m1} and \code{m2} are fit by
  maximum likelihood (ML) and/or by restricted maximum likelihood (REML)
  to each of the simulated data vectors.
}
\value{
  an object of class \code{simulate.lme} with components \code{null} and
  \code{alt}.  Each of these has components \code{ML} and/or \code{REML}
  which are matrices.  An attribute called \code{Random.seed} contains
  the seed that was used for the random number generator.
}
\references{
 Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
 in S and S-PLUS", Springer.  
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{lme}}, \code{\link{set.seed}}}
\examples{
\dontrun{
orthSim <-
   simulate.lme(list(fixed = distance ~ age, data = Orthodont,
                     random = ~ 1 | Subject), nsim = 1000,
                m2 = list(random = ~ age | Subject))
}
}
\keyword{models}
