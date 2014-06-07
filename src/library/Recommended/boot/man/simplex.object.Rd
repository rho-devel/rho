\name{simplex.object}
\alias{simplex.object}
\title{
Linear Programming Solution Objects
}
\description{
  Class of objects that result from solving a linear programming
  problem using \code{simplex}.
}
\section{Generation}{
This class of objects is returned from calls to the function \code{simplex}.
}
\section{Methods}{
The class \code{"saddle.distn"} has a method for the function \code{print}.
}
\section{Structure}{
  Objects of class \code{"simplex"} are implemented as a list with the
  following components.

  \describe{
    \item{soln}{
      The values of \code{x} which optimize the objective function under
      the specified constraints provided those constraints are jointly feasible.
    }
    \item{solved}{
      This indicates whether the problem was solved.  A value of \code{-1}
      indicates that no feasible solution could be found.  A value of
      \code{0} that the maximum number of iterations was reached without
      termination of the second stage.  This may indicate an unbounded
      function or simply that more iterations are needed. A value of
      \code{1} indicates that an optimal solution has been found.
    }
    \item{value}{
      The value of the objective function at \code{soln}.
    }
    \item{val.aux}{
      This is \code{NULL} if a feasible solution is found. Otherwise it is
      a positive value giving the value of the auxiliary objective
      function when it was minimized.
    }
    \item{obj}{
      The original coefficients of the objective function.
    }
    \item{a}{
      The objective function coefficients re-expressed such that the basic
      variables have coefficient zero.
    }
    \item{a.aux}{
      This is \code{NULL} if a feasible solution is found. Otherwise it is the
      re-expressed auxiliary objective function at the termination of the first
      phase of the simplex method.
    }
    \item{A}{
      The final constraint matrix which is expressed in terms of the
      non-basic variables.  If a feasible solution is found then this will
      have dimensions \code{m1+m2+m3} by \code{n+m1+m2}, where the final
      \code{m1+m2} columns correspond to slack and surplus variables.  If
      no feasible solution is found there will be an additional
      \code{m1+m2+m3} columns for the artificial variables introduced to
      solve the first phase of the problem.
    }
    \item{basic}{
      The indices of the basic (non-zero) variables in the solution.
      Indices between \code{n+1} and \code{n+m1} correspond to slack
      variables, those between \code{n+m1+1} and \code{n+m2} correspond to
      surplus variables and those greater than \code{n+m2} are artificial
      variables.  Indices greater than \code{n+m2} should occur only if
      \code{solved} is \code{-1} as the artificial variables are discarded in
      the second stage of the simplex method.
    }
    \item{slack}{
      The final values of the \code{m1} slack variables which arise when
      the "<=" constraints are re-expressed as the equalities
      \code{A1\%*\%x + slack = b1}.
    }
    \item{surplus}{
      The final values of the \code{m2} surplus variables which arise when
      the "<=" constraints are re-expressed as the equalities \code{A2\%*\%x -
        surplus = b2}.
    }
    \item{artificial}{
      This is NULL if a feasible solution can be found.  If no solution
      can be found then this contains the values of the \code{m1+m2+m3}
      artificial variables which minimize their sum subject to the
      original constraints.  A feasible solution exists only if all of the
      artificial variables can be made 0 simultaneously.
    }
  }
}
\seealso{
  \code{\link{print.simplex}}, \code{\link{simplex}}
}
\keyword{optimize}
\keyword{methods}
% Converted by Sd2Rd version 0.3-1.
