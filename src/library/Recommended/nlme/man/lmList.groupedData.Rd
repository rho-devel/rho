% File nlme/man/lmList.groupedData.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: lmList.groupedData.Rd,v 1.7.2.1 2003/04/17 22:28:45 bates Exp $
\name{lmList.groupedData}
\title{lmList Fit from a groupedData Object}
\usage{
\method{lmList}{groupedData}(object, data, level, subset, na.action, pool)
}
\alias{lmList.groupedData}
\arguments{
  \item{object}{a data frame inheriting from class \code{groupedData}.}
  \item{data}{this argument is included for consistency with the generic
    function. It is ignored in this method function.}
  \item{level}{
    an optional integer specifying the level of grouping to be used when 
    multiple nested levels of grouping are present.
  }
  \item{subset}{an optional expression indicating which subset of the rows of
    \code{data} should  be  used in the fit. This can be a logical
    vector, or a numeric vector indicating which observation numbers are
    to be included, or a  character  vector of the row names to be
    included.  All observations are included by default.}
  \item{na.action}{a function that indicates what should happen when the
    data contain \code{NA}s.  The default action (\code{na.fail}) causes
    \code{lmList} to print an error message and terminate if there are any
    incomplete observations.
  }
  \item{pool}{
    an optional logical value that is preserved as an attribute of the
    returned value.  This will be used as the default for \code{pool} in
    calculations of standard deviations or standard errors for summaries.
  }
}
\description{
  The response variable and primary covariate in \code{formula(object)}
  are used to construct the linear model formula. This formula
  and the \code{groupedData} object are passed as the \code{object} and
  \code{data} arguments to \code{lmList.formula}, together with any other
  additional arguments in the function call. See the documentation on
  \code{lmList.formula} for a description of that function.
}
\value{
  a list of \code{lm} objects with as many components as the number of
  groups defined by the grouping factor. Generic functions such as
  \code{coef}, \code{fixed.effects}, \code{lme}, \code{pairs},
  \code{plot}, \code{predict}, \code{random.effects}, \code{summary},
  and \code{update} have methods that can be applied to an \code{lmList}
  object. 
}

\seealso{
  \code{\link{groupedData}}, \code{\link{lm}},
  \code{\link{lme.lmList}}, \code{\link{lmList}},
  \code{\link{lmList.formula}}
}
\examples{
fm1 <- lmList(Orthodont)
summary(fm1)
}
\keyword{models}
