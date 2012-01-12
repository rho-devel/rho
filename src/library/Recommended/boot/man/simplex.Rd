\name{simplex}
\alias{simplex}
\title{
Simplex Method for Linear Programming Problems
}
\description{
  This function will optimize the linear function \code{a\%*\%x} subject
  to the constraints \code{A1\%*\%x <= b1}, \code{A2\%*\%x >= b2},
  \code{A3\%*\%x = b3} and \code{x >= 0}.  Either maximization or
  minimization is possible but the default is minimization.
}
\usage{
simplex(a, A1 = NULL, b1 = NULL, A2 = NULL, b2 = NULL, A3 = NULL,
        b3 = NULL, maxi = FALSE, n.iter = n + 2 * m, eps = 1e-10)
}
\arguments{
  \item{a}{
    A vector of length \code{n} which gives the coefficients of the
    objective function.
  }
  \item{A1}{
    An \code{m1} by \code{n} matrix of coefficients for the \eqn{\leq}{<=} type of
    constraints.
  }
  \item{b1}{
    A vector of length \code{m1} giving the right hand side of the \eqn{\leq}{<=}
    constraints. This argument is required if \code{A1} is given and
    ignored otherwise.  All values in \code{b1} must be non-negative.
  }
  \item{A2}{
    An \code{m2} by \code{n} matrix of coefficients for the \eqn{\geq}{>=} type of
    constraints.
  }
  \item{b2}{
    A vector of length \code{m2} giving the right hand side of the \eqn{\geq}{>=}
    constraints. This argument is required if \code{A2} is given and
    ignored otherwise.  All values in \code{b2} must be non-negative.
    Note that the constraints \code{x >= 0} are included automatically
    and so should not be repeated here.
  }
  \item{A3}{
    An \code{m3} by \code{n} matrix of coefficients for the equality
    constraints.
  }
  \item{b3}{
    A vector of length \code{m3} giving the right hand side of equality
    constraints. This argument is required if \code{A3} is given and
    ignored otherwise.  All values in \code{b3} must be non-negative.
  }
  \item{maxi}{
    A logical flag which specifies minimization if \code{FALSE}
    (default) and maximization otherwise.  If \code{maxi} is \code{TRUE}
    then the maximization problem is recast as a minimization problem by
    changing the objective function coefficients to their negatives.
  }
  \item{n.iter}{
    The maximum number of iterations to be conducted in each phase of
    the simplex method.  The default is \code{n+2*(m1+m2+m3)}.
  }
  \item{eps}{
    The floating point tolerance to be used in tests of equality.
  }
}
\value{
  An object of class \code{"simplex"}: see \code{\link{simplex.object}}.
}
\details{
  The method employed by this function is the two phase tableau simplex
  method. If there are \eqn{\geq}{>=} or equality constraints an initial feasible
  solution is not easy to find.  To find a feasible solution an
  artificial variable is introduced into each \eqn{\geq}{>=} or equality
  constraint and an auxiliary objective function is defined as the sum
  of these artificial variables.  If a feasible solution to the set of
  constraints exists then the auxiliary objective will be minimized when
  all of the artificial variables are 0. These are then discarded and
  the original problem solved starting at the solution to the auxiliary
  problem.  If the only constraints are of the \eqn{\leq}{<=} form, the origin is
  a feasible solution and so the first stage can be omitted.
}
\note{
  The method employed here is suitable only for relatively small
  systems.  Also if possible the number of constraints should be reduced
  to a minimum in order to speed up the execution time which is
  approximately proportional to the cube of the number of constraints.
  In particular if there are any constraints of the form \code{x[i] >=
    b2[i]} they should be omitted by setting \code{x[i] = x[i]-b2[i]},
  changing all the constraints and the objective function accordingly
  and then transforming back after the solution has been found.
}
\references{
  Gill, P.E., Murray, W. and Wright, M.H. (1991)
  \emph{Numerical Linear Algebra and Optimization Vol. 1}. Addison-Wesley.

  Press, W.H., Teukolsky, S.A., Vetterling, W.T. and Flannery, B.P. (1992)
  \emph{Numerical Recipes: The Art of Scientific Computing (Second Edition)}.
  Cambridge University Press.
}
\examples{
# This example is taken from Exercise 7.5 of Gill, Murray and Wright (1991).
enj <- c(200, 6000, 3000, -200)
fat <- c(800, 6000, 1000, 400)
vitx <- c(50, 3, 150, 100)
vity <- c(10, 10, 75, 100)
vitz <- c(150, 35, 75, 5)
simplex(a = enj, A1 = fat, b1 = 13800, A2 = rbind(vitx, vity, vitz),
        b2 = c(600, 300, 550), maxi = TRUE)
}
\keyword{optimize}
