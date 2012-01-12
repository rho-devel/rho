% File nlme/man/lmList.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: lmList.Rd,v 1.7.2.1 2003/04/17 22:28:45 bates Exp $
\name{lmList}
\title{List of lm Objects with a Common Model}
\usage{
lmList(object, data, level, subset, na.action, pool)
\method{update}{lmList}(object, formula., \dots, evaluate = TRUE)
\method{print}{lmList}(x, pool, \dots)
}
\alias{lmList}
\alias{lmList.formula}
\alias{print.lmList}
\alias{update.lmList}
\arguments{
  \item{object}{For \code{lmList},
    either a linear formula object of the form \code{y ~ x1+...+xn | g}
    or a \code{groupedData} object. In the formula object, \code{y}
    represents the response, \code{x1,...,xn} the covariates, and
    \code{g} the grouping factor specifying the partitioning of the data
    according to which different \code{lm} fits should be performed. The
    grouping factor \code{g} may be omitted from the formula, in which
    case the grouping structure will be obtained from \code{data}, which
    must inherit from class \code{groupedData}. The method function 
    \code{lmList.groupedData} is documented separately.
    For the method \code{update.lmList}, \code{object} is an object
    inheriting from class \code{lmList}.
  }
  \item{formula}{(used in \code{update.lmList} only)
    a two-sided linear formula with the common model for the individuals
    \code{lm} fits.
    }
 \item{formula.}{Changes to the formula -- see \code{update.formula} for
   details.}
  \item{data}{
    a data frame in which to interpret the variables named in
    \code{object}. 
  }
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
   an optional logical value indicating whether a pooled
   estimate of the residual standard error should be used 
   in calculations of standard deviations or standard errors for
   summaries.
 }
 \item{x}{an object inheriting from class \code{lmList} to be printed.}
 \item{\dots}{some methods for this generic require additional
   arguments.  None are used in this method.}
 \item{evaluate}{If \code{TRUE} evaluate the new call else return the call.}
}
\description{
  \code{Data} is partitioned according to the levels of the grouping
  factor \code{g} and individual \code{lm} fits are obtained for each
  \code{data} partition, using the model defined in \code{object}.
}

\value{
  a list of \code{lm} objects with as many components as the number of
  groups defined by the grouping factor. Generic functions such as
  \code{coef}, \code{fixed.effects}, \code{lme}, \code{pairs},
  \code{plot}, \code{predict}, \code{random.effects}, \code{summary},
  and \code{update} have methods that can be applied to an \code{lmList}
  object. 
}
\references{
  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer.  
}

\seealso{
  \code{\link{lm}},
  \code{\link{lme.lmList}}, 
  \code{\link{plot.lmList}},  
  \code{\link{pooledSD}}, 
  \code{\link{predict.lmList}}, 
  \code{\link{residuals.lmList}}, 
  \code{\link{summary.lmList}}    
}
\examples{
fm1 <- lmList(distance ~ age | Subject, Orthodont)
summary(fm1)
}
\keyword{models}

