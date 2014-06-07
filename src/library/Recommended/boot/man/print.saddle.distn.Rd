\name{print.saddle.distn}
\alias{print.saddle.distn}
\title{
Print Quantiles of Saddlepoint Approximations
}
\description{
This is a method for the function \code{print()} to print objects of class
\code{"saddle.distn"}.
}
\usage{
\method{print}{saddle.distn}(x, \dots)
}
\arguments{
  \item{x}{
    An object of class \code{"saddle.distn"} created by a call to
    \code{saddle.distn}.
  }
  \item{\dots}{further arguments passed to or from other methods.}
 
}
\value{
The input is returned invisibly.
}
\details{
The quantiles of the saddlepoint approximation to the distribution are printed
along with the original call and some other useful information.
}
\seealso{
\code{\link{lines.saddle.distn}}, \code{\link{saddle.distn}}
}
\keyword{print}
\keyword{smooth}
\keyword{nonparametric}
% Converted by Sd2Rd version 0.3-1.
