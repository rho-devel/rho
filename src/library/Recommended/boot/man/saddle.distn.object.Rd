\name{saddle.distn.object}
\alias{saddle.distn.object}
\title{
Saddlepoint Distribution Approximation Objects
}
\description{
  Class of objects that result from calculating saddlepoint distribution
  approximations by a call to \code{saddle.distn}.
}
\section{Generation}{
  This class of objects is returned from calls to the function
  \code{\link{saddle.distn}}.
}
\section{Methods}{
  The class \code{"saddle.distn"} has methods for the functions
  \code{\link{lines}} and \code{\link{print}}.
}
\section{Structure}{
  Objects of class \code{"saddle.distn"} are implemented as a list with
  the following components.
  \describe{
    \item{quantiles}{
      A matrix with 2 columns.  The first column contains the
      probabilities \code{alpha} and the second column contains the
      estimated quantiles of the distribution at those probabilities
      derived from the spline.
    }
    \item{points}{
      A matrix of evaluations of the saddlepoint approximation.  The first
      column contains the values of \code{t} which were used, the second
      and third contain the density and cdf approximations at those points
      and the rest of the columns contain the solutions to the saddlepoint
      equations.  When \code{type} is \code{"simp"}, there is only one of
      those.  When \code{type} is \code{"cond"} there are \code{2*d-1}
      where \code{d} is the number of columns in \code{A} or the output of
      \code{A(t,\dots{})}. The first \code{d} of these correspond to the
      numerator and the remainder correspond to the denominator.
    }
    \item{distn}{
      An object of class \code{smooth.spline}.  This corresponds to the
      spline fitted to the saddlepoint cdf approximations in points in
      order to approximate the entire distribution.  For the structure of
      the object see \code{smooth.spline}.
    }
    \item{call}{
      The original call to \code{saddle.distn} which generated the object.
    }
    \item{LR}{
      A logical variable indicating whether the Lugananni-Rice
      approximations were used.
    }
  }
}
\seealso{
  \code{\link{lines.saddle.distn}}, \code{\link{saddle.distn}},
  \code{\link{print.saddle.distn}}
}
\keyword{nonparametric}
\keyword{methods}
\keyword{smooth}
