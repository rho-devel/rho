% File nlme/man/Names.formula.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: Names.formula.Rd,v 1.6 2002/03/05 14:59:39 bates Exp $
\name{Names.formula}
\title{Extract Names from a formula}
\usage{
\method{Names}{formula}(object, data, exclude, \dots)
}
\alias{Names.formula}
\alias{Names.listForm}
\arguments{
 \item{object}{an object inheriting from class \code{formula}.}
 \item{data}{an optional data frame containing the variables specified
   in \code{object}. By default the variables are taken from the
   environment from which \code{Names.formula} is called.}
 \item{exclude}{an optional character vector with names to be excluded
   from the returned value. Default is \code{c("pi",".")}.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function returns the names of the terms corresponding to
  the right hand side of \code{object} (treated as a linear formula),
  obtained as the column names of the corresponding
  \code{model.matrix}. 
}
\value{
  a character vector with the column names of the \code{model.matrix}
  corresponding to the right hand side of \code{object} which are not
  listed in \code{excluded}.  
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{model.matrix}}, \code{\link{terms}},
  \code{\link{Names}}
}

\examples{
Names(distance ~ Sex * age, data = Orthodont)
}
\keyword{models}
