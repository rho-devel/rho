% File nlme/man/getCovariate.data.frame.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: getCovariate.data.frame.Rd,v 1.7 2002/03/05 14:59:39 bates Exp $
\name{getCovariate.data.frame}
\title{Extract Data Frame Covariate}
\usage{
\method{getCovariate}{data.frame}(object, form, data)
}
\alias{getCovariate.data.frame}
\arguments{
 \item{object}{an object inheriting from class \code{data.frame}.}
 \item{form}{an optional formula specifying the covariate to be
   evaluated in \code{object}. Defaults to \code{formula(object)}.}
 \item{data}{some methods for this generic require a separate data
   frame. Not used in this method.}
}
\description{
  The right hand side of \code{form}, stripped of any conditioning
  expression (i.e. an expression following a \code{|} operator), is
  evaluated in \code{object}.  
}
\value{
  the value of the right hand side of \code{form}, stripped of
  any conditional expression, evaluated in \code{object}.
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{getCovariateFormula}}}

\examples{
getCovariate(Orthodont)
}
\keyword{models}
