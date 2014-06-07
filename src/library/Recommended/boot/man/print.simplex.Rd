\name{print.simplex}
\alias{print.simplex}
\title{
Print Solution to Linear Programming Problem
}
\description{
This is a method for the function \code{print()} to print objects of class
\code{"simplex"}.
}
\usage{
\method{print}{simplex}(x, \dots)
}
\arguments{
  \item{x}{
    An object of class \code{"simplex"} created by calling the
    function \code{simplex} to solve a linear programming problem.
  }
  \item{\dots}{further arguments passed to or from other methods.}
}
\value{
\code{x} is returned silently.
}
\details{
The coefficients of the objective function are printed.
If a solution to the linear programming problem was found then the solution
and the optimal value of the objective function are printed.  If a feasible
solution was found but the maximum number of iterations was exceeded then the
last feasible solution and the objective function value at that point are
printed.  If no feasible solution could be found then a message stating that is
printed.
}
\seealso{
\code{\link{simplex}}
}
\keyword{print}
\keyword{optimize}
% Converted by Sd2Rd version 0.3-1.
