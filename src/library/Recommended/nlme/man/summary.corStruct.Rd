% File nlme/man/summary.corStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{summary.corStruct}
\title{Summarize a corStruct Object}
\usage{
\method{summary}{corStruct}(object, structName, \dots)
}
\alias{summary.corStruct}
\alias{summary.corAR1}
\alias{summary.corARMA}
\alias{summary.corCAR1}
\alias{summary.corCompSymm}
\alias{summary.corExp}
\alias{summary.corGaus}
\alias{summary.corIdent}
\alias{summary.corLin}
\alias{summary.corNatural}
\alias{summary.corRatio}
\alias{summary.corSpher}
\alias{summary.corSymm}

\arguments{
 \item{object}{an object inheriting from class \code{corStruct},
   representing a correlation structure.}
 \item{structName}{an optional character string defining the type of
   correlation structure associated with \code{object}, to be used in
   the \code{print.summary} method. Defaults to
   \code{class(object)[1]}.} 
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function prepares \code{object} to be printed using the
  \code{print.summary} method, by changing its class and adding a
  \code{structName} attribute to it.
}
\value{
  an object identical to \code{object}, but with its class changed to
  \code{summary.corStruct} and an additional attribute
  \code{structName}. The returned value inherits from the same classes
  as \code{object}.
}
\author{Jose Pinheiro and Douglas Bates }
\seealso{
  \code{\link{corClasses}}, 
  \code{\link{corNatural}},
  \code{\link{Initialize.corStruct}}, 
  \code{\link{summary}} 
}

%\seealso{\code{\link{print.summary.corStruct}}}

\examples{
cs1 <- corAR1(0.2)
summary(cs1)
}
\keyword{models}
