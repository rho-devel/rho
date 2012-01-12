% File nlme/man/gnlsStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: gnlsStruct.Rd,v 1.5.2.1 2002/08/09 19:45:29 bates Exp $
\name{gnlsStruct}
\title{Generalized Nonlinear Least Squares Structure}
\usage{
gnlsStruct(corStruct, varStruct)
}
\alias{gnlsStruct}
\alias{Initialize.gnlsStruct}
\arguments{
 \item{corStruct}{an optional \code{corStruct} object, representing a
   correlation structure. Default is \code{NULL}.}
 \item{varStruct}{an optional \code{varFunc} object, representing a
   variance function structure. Default is \code{NULL}.}
}
\description{
  A generalized nonlinear least squares structure is a list of model
  components representing different sets of parameters in the nonlinear 
  model. A \code{gnlsStruct}  may contain \code{corStruct} and
  \code{varFunc} objects. \code{NULL} arguments are not included in the
  \code{gnlsStruct} list. 
}
\value{
  a list of model variance-covariance components determining the
  parameters to be estimated for the associated nonlinear model.
}

\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}

\seealso{\code{\link{gnls}}, \code{\link{corClasses}},
  \code{\link{residuals.gnlsStruct}}
  \code{\link{varFunc}}
} 

\examples{
gnls1 <- gnlsStruct(corAR1(), varPower())
}
\keyword{models}
