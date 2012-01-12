% File nlme/man/coef.corStruct.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: coef.corStruct.Rd,v 1.6.2.3 2003/08/09 22:45:17 bates Exp $
\name{coef.corStruct}
\title{Coefficients of a corStruct Object}
\usage{
\method{coef}{corStruct}(object, unconstrained, \dots)
\method{coef}{corStruct}(object, \dots) <- value
}
\alias{coef.corStruct}
\alias{coef.corAR1}
\alias{coef.corARMAd}   
\alias{coef.corCAR1}   
\alias{coef.corCompSymm}
\alias{coef.corHF}
\alias{coef.corIdent}  
\alias{coef.corLin}
\alias{coef.corNatural}
\alias{coef.corSpatial}
\alias{coef.corSpher}
\alias{coef.corSymm}
\alias{coef<-.corStruct}
\alias{coef<-.corAR1}
\alias{coef<-.corARMA}   
\alias{coef<-.corCAR1}   
\alias{coef<-.corCompSymm}
\alias{coef<-.corNatural}
\alias{coef<-.corHF}
\alias{coef<-.corIdent}  
\alias{coef<-.corLin}
\alias{coef<-.corSpatial}
\alias{coef<-.corSpher}
\alias{coef<-.corSymm}
\alias{coef.summary.nlsList}

\arguments{
 \item{object}{an object inheriting from class \code{corStruct},
   representing a correlation structure.}
 \item{unconstrained}{a logical value. If \code{TRUE} the coefficients
   are returned in unconstrained form (the same used in the optimization
   algorithm). If \code{FALSE} the coefficients are returned in
   "natural", possibly constrained, form. Defaults to \code{TRUE}.}
 \item{value}{a vector with the replacement values for the coefficients
   associated with \code{object}. It must be a vector with the same length
   of \code{coef{object}} and must be given in unconstrained form.}
 \item{\dots}{some methods for this generic require additional
    arguments.  None are used in this method.} 
}
\description{
  This method function extracts the coefficients associated with the
  correlation structure represented by \code{object}. 
}
\value{
  a vector with the coefficients corresponding to \code{object}.
}
\references{
   Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
     and S-PLUS}, Springer, New York.  
}
\author{Jose Pinheiro and Douglas Bates }
\section{SIDE EFFECTS}{
  On the left side of an assignment, sets the values of the coefficients
  of \code{object} to \code{value}. \code{Object} must be initialized (using
  \code{Initialize}) before new values can be assigned to its
  coefficients. 
}

\seealso{\code{\link{corAR1}}, \code{\link{corARMA}},
    \code{\link{corCAR1}}, \code{\link{corCompSymm}},
    \code{\link{corExp}}, \code{\link{corGaus}}, \code{\link{corLin}},
    \code{\link{corRatio}}, \code{\link{corSpatial}}, \code{\link{corSpher}},
    \code{\link{corSymm}},\code{\link{Initialize}}}
\examples{
cst1 <- corARMA(p = 1, q = 1)
coef(cst1)
}
\keyword{models}
