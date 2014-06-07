% File nlme/man/nlsList.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: nlsList.Rd,v 1.7 2002/01/28 03:44:17 bates Exp $
\name{nlsList}
\title{List of nls Objects with a Common Model}
\usage{
nlsList(model, data, start, control, level, subset, na.action, pool)
\method{update}{nlsList}(object, model., \dots, evaluate = TRUE)
}
\alias{nlsList}
\alias{nlsList.formula}
\alias{update.nlsList}
\arguments{
  \item{object}{an object inheriting from class \code{nlsList}, representing
      a list of fitted \code{nls} objects.}
  \item{model}{either a nonlinear model formula, with the response on
    the left of a \code{~} operator and an expression involving
    parameters, covariates, and a grouping factor separated by the
    \code{|} operator on the right, or a \code{selfStart} function.  The
    method function \code{nlsList.selfStart} is documented separately.
  }
 \item{model.}{Changes to the model -- see \code{update.formula} for
   details.}
  \item{data}{
    a data frame in which to interpret the variables named in
    \code{model}.
  }
  \item{start}{an optional named list with initial values for the
    parameters to be estimated in \code{model}. It is passed as the
    \code{start} argument to each \code{nls} call and is required when
    the nonlinear function in \code{model} does not inherit from class
    \code{selfStart}.
  }
  \item{control}{a list of control values passed as the \code{control}
    argument to \code{nls}. Defaults to an empty list.
  }
  \item{level}{an optional integer specifying the level of grouping to
    be used when multiple nested levels of grouping are present.}
 \item{subset}{an optional expression indicating the subset of the rows of
   \code{data} that should be used in the fit. This can be a logical
   vector, or a numeric vector indicating which observation numbers are
   to be included, or a  character  vector of the row names to be
   included.  All observations are included by default.}
  \item{na.action}{a function that indicates what should happen when the
    data contain \code{NA}s.  The default action (\code{na.fail}) causes
    \code{nlsList} to print an error message and terminate if there are any
    incomplete observations.
  }
  \item{pool}{
    an optional logical value that is preserved as an attribute of the
    returned value.  This will be used as the default for \code{pool} in
    calculations of standard deviations or standard errors for summaries.
  }
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
 \item{evaluate}{If \code{TRUE} evaluate the new call else return the call.}
}
\description{
  \code{Data} is partitioned according to the levels of the grouping
  factor defined in \code{model} and individual \code{nls} fits are
  obtained for each \code{data} partition, using the model defined in
  \code{model}.
}
\value{
  a list of \code{nls} objects with as many components as the number of
  groups defined by the grouping factor. Generic functions such as
  \code{coef}, \code{fixed.effects}, \code{lme}, \code{pairs},
  \code{plot}, \code{predict}, \code{random.effects}, \code{summary},
  and \code{update} have methods that can be applied to an \code{nlsList}
  object. 
}
\references{
 Pinheiro, J.C., and Bates, D.M. (2000), \emph{Mixed-Effects Models
 in S and S-PLUS}, Springer.  
}
\seealso{
  \code{\link{nls}}, \code{\link{nlme.nlsList}},
  \code{\link{nlsList.selfStart}}, 
  \code{\link{summary.nlsList}}
}
\examples{
fm1 <- nlsList(uptake ~ SSasympOff(conc, Asym, lrc, c0),
   data = CO2, start = c(Asym = 30, lrc = -4.5, c0 = 52))
summary(fm1)
}
\keyword{models}

