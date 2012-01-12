% File nlme/man/coef.lmList.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: coef.lmList.Rd,v 1.7.2.1 2003/04/17 22:28:45 bates Exp $
\name{coef.lmList}
\title{Extract lmList Coefficients}
\usage{
\method{coef}{lmList}(object, augFrame, data, which, FUN,
   omitGroupingFactor, \dots)
}
\alias{coef.lmList}
\arguments{
 \item{object}{an object inheriting from class \code{lmList}, representing
   a list of \code{lm} objects with a common model.
 }
 \item{augFrame}{an optional logical value. If \code{TRUE}, the returned
   data frame is augmented with variables defined in the data frame used
   to produce \code{object}; else, if \code{FALSE}, only the coefficients
   are returned. Defaults to \code{FALSE}.
 }
 \item{data}{an optional data frame with the variables to be used for
   augmenting the returned data frame when \code{augFrame =
     TRUE}. Defaults to the data frame used to fit \code{object}.}
 \item{which}{an optional positive integer or character vector
   specifying which columns of the data frame used to produce \code{object}
   should be used in the augmentation of the returned data
   frame. Defaults to all variables in the data.
 }
 \item{FUN}{an optional summary function or a list of summary functions
   to be applied to group-varying variables, when collapsing the data
   by groups.  Group-invariant variables are always summarized by the
   unique value that they assume within that group. If \code{FUN} is a
   single function it will be applied to each non-invariant variable by
   group to produce the summary for that variable.  If \code{FUN} is a
   list of functions, the names in the list should designate classes of
   variables in the frame such as \code{ordered}, \code{factor}, or
   \code{numeric}.  The indicated function will be applied to any
   group-varying variables of that class.  The default functions to be
   used are \code{mean} for numeric factors, and \code{Mode} for both
   \code{factor} and \code{ordered}.  The \code{Mode} function, defined
   internally in \code{gsummary}, returns the modal or most popular
   value of the variable.  It is different from the \code{mode} function
   that returns the S-language mode of the variable.
 }
 \item{omitGroupingFactor}{an optional logical value.  When \code{TRUE}
   the grouping factor itself will be omitted from the group-wise
   summary of \code{data} but the levels of the grouping factor will
   continue to be used as the row names for the returned data frame.
   Defaults to \code{FALSE}.
 }
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  The coefficients of each \code{lm} object in the \code{object} list are
  extracted and organized into a data frame, with rows corresponding to
  the \code{lm} components and columns corresponding to the
  coefficients.  Optionally, the returned data frame may be augmented
  with covariates summarized over the groups associated with the
  \code{lm} components.  
}
\value{
  a data frame inheriting from class \code{coef.lmList} with the estimated
  coefficients for each \code{lm} component of \code{object} and,
  optionally, other covariates summarized over the groups corresponding
  to the \code{lm} components. The returned object also inherits from
  classes \code{ranef.lmList} and \code{data.frame}.
}

\references{
   Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
     and S-PLUS}, Springer, New York, esp. pp. 457-458.   
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{lmList}}, \code{\link{fixed.effects.lmList}},
  \code{\link{ranef.lmList}},
  \code{\link{plot.ranef.lmList}}, \code{\link{gsummary}}} 

\examples{
fm1 <- lmList(distance ~ age|Subject, data = Orthodont)
coef(fm1)
coef(fm1, augFrame = TRUE)
}
\keyword{models}
