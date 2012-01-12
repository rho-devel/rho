% File nlme/man/Glucose.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Glucose}
\alias{Glucose}
\title{Glucose levels over time}
\description{
    The \code{Glucose} data frame has 378 rows and 4 columns.
}
\format{
    This data frame contains the following columns:
    \describe{
        \item{Subject}{
            an ordered factor with levels
            \code{6} < \code{2} < \code{3} < \code{5} < \code{1} < \code{4}
        }
        \item{Time}{
            a numeric vector
        }
        \item{conc}{
            a numeric vector of glucose levels
        }
        \item{Meal}{
            an ordered factor with levels
            \code{2am} < \code{6am} < \code{10am} < \code{2pm} < \code{6pm} < \code{10pm}
        }
    }
}
\source{
    Hand, D. and Crowder, M. (1996), \emph{Practical Longitudinal Data
        Analysis}, Chapman and Hall, London.
}
%\examples{}
\keyword{datasets}
