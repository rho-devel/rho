\name{print.bootci}
\alias{print.bootci}
\title{
Print Bootstrap Confidence Intervals
}
\description{
This is a method for the function \code{print()} to print objects of the
class \code{"bootci"}.
}
\usage{
\method{print}{bootci}(x, hinv = NULL, ...)
}
\arguments{
  \item{x}{
    The output from a call to \code{boot.ci}.
  }
  \item{hinv}{
    A transformation to be made to the interval end-points before they are
    printed.
  }
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
The object \code{ci.out} is returned invisibly.
}
\details{
This function prints out the results from \code{boot.ci} in a "nice" format.
It also notes whether the scale of the intervals is the original scale
of the input to \code{boot.ci} or a different scale and whether the calculations
were done on a transformed scale.  It also looks at the order statistics
that were used in calculating the intervals.  If the smallest or largest
values were used then it prints a message 


\code{Warning : Intervals used Extreme Quantiles}


Such intervals should be considered very unstable and not relied upon for
inferences.  Even if the extreme values are not used, it is possible that the
intervals are unstable if they used quantiles close to the extreme values.
The function alerts the user to intervals which use the upper
or lower 10 order statistics with the message


\code{Some intervals may be unstable}
}
\seealso{
\code{\link{boot.ci}}
}
\keyword{print}
\keyword{htest}
% Converted by Sd2Rd version 0.3-1.
