% File nlme/man/nlsList.selfStart.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: nlsList.selfStart.Rd,v 1.7 2002/01/28 03:44:17 bates Exp $
\name{nlsList.selfStart}
\title{nlsList Fit from a selfStart Function}
\usage{
\method{nlsList}{selfStart}(model, data, start, control, level, subset, na.action, pool)
}
\alias{nlsList.selfStart}
\arguments{
  \item{model}{a \code{selfStart} model function, which calculates
    initial estimates for the model parameters from \code{data}.}
  \item{data}{a data frame in which to interpret the variables in
    \code{model}. Because no grouping factor can be specified in
    \code{model}, \code{data} must inherit from class
    \code{groupedData}. 
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
}
\description{
  The response variable and primary covariate in \code{formula(data)}
  are used together with \code{model} to construct the nonlinear model
  formula. This is used in the \code{nls} calls and, because a
  selfStarting model function can calculate initial estimates for its
  parameters from the data, no starting estimates need to be provided.
}
\value{
  a list of \code{nls} objects with as many components as the number of
  groups defined by the grouping factor. A \code{NULL} value is assigned
  to the components corresponding to clusters for which the \code{nls}
  algorithm failed to converge. Generic functions such as \code{coef},
  \code{fixed.effects}, \code{lme}, \code{pairs}, \code{plot},
  \code{predict}, \code{random.effects}, \code{summary}, and
  \code{update} have methods that can be applied to an \code{nlsList}
  object.  
}

\seealso{
  \code{\link{selfStart}}, \code{\link{groupedData}},
  \code{\link{nls}}, \code{\link{nlsList}},
  \code{\link{nlme.nlsList}}, \code{\link{nlsList.formula}}
}
\examples{
fm1 <- nlsList(SSasympOff, CO2)
summary(fm1)
}
\keyword{models}
