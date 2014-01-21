% File nlme/man/Initialize.glsStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: Initialize.glsStruct.Rd,v 1.1.2.1 2002/08/09 19:45:29 bates Exp $
\name{Initialize.glsStruct}
\title{Initialize a glsStruct Object}
\usage{
\method{Initialize}{glsStruct}(object, data, control, \dots)
}
\alias{Initialize.glsStruct}
\arguments{
 \item{object}{an object inheriting from class \code{glsStruct},
   representing a list of linear model components, such as
   \code{corStruct} and \code{varFunc} objects.}
 \item{data}{a data frame in which to evaluate the variables defined in
   \code{formula(object)}.}
 \item{control}{an optional list with control parameters for the
   initialization and optimization algorithms used in
   \code{gls}. Defaults to \code{list(singular.ok = FALSE, qrTol =
     .Machine$single.eps)}, implying that linear dependencies are not
   allowed in the model and that the tolerance for detecting linear
   dependencies among the columns of the regression matrix is
   \code{.Machine$single.eps}.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  The individual linear model components of the \code{glsStruct} list
  are initialized.  
}
\value{
  a \code{glsStruct} object similar to \code{object}, but with
  initialized model components. 
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{gls}},
  \code{\link{Initialize.corStruct}},
  \code{\link{Initialize.varFunc}},
  \code{\link{Initialize}}}
\keyword{models}
