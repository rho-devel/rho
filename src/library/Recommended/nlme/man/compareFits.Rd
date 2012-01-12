% File nlme/man/compareFits.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: compareFits.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{compareFits}
\title{Compare Fitted Objects}
\usage{
compareFits(object1, object2, which)
}
\alias{compareFits}
\alias{print.compareFits}
\arguments{
 \item{object1,object2}{data frames, or matrices, with the same
   row names, but possibly different column names. These will usually
   correspond to coefficients from fitted objects with a grouping
   structure (e.g. \code{lme} and \code{lmList} objects).}
 \item{which}{an optional integer or character vector indicating which
   columns in \code{object1} and \code{object2} are to be used in the
   returned object. Defaults to all columns.}
}
\description{
  The columns in \code{object1} and \code{object2} are put together in
  matrices which allow direct comparison of the individual elements for
  each object. Missing columns in either object are replaced by
  \code{NA}s.  
}
\value{
 a three-dimensional array, with the third dimension given by the number
 of unique column names in either \code{object1} or \code{object2}. To
 each column name there corresponds a matrix with as many rows as the
 rows in \code{object1} and two columns, corresponding to \code{object1}
 and \code{object2}. The returned object inherits from class
 \code{compareFits}. 
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{
  \code{\link{plot.compareFits}},
  \code{\link{pairs.compareFits}},
  \code{\link{comparePred}},
  \code{\link{coef}},
  \code{\link{random.effects}}
}
\examples{
fm1 <- lmList(Orthodont)
fm2 <- lme(fm1)
compareFits(coef(fm1), coef(fm2))
}
\keyword{models}
