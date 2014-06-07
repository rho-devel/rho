% File nlme/man/Tetracycline2.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Tetracycline2}
\alias{Tetracycline2}
\title{Pharmacokinetics of tetracycline}
\description{
    The \code{Tetracycline2} data frame has 40 rows and 4 columns.
}
\format{
    This data frame contains the following columns:
    \describe{
        \item{conc}{
            a numeric vector
        }
        \item{Time}{
            a numeric vector
        }
        \item{Subject}{
            an ordered factor with levels
            \code{4} < \code{5} < \code{2} < \code{1} < \code{3}
        }
        \item{Formulation}{
            a factor with levels
            \code{Berkmycin} 
            \code{tetramycin} 
        }
    }
}
\source{
    Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
        and S-PLUS}, Springer, New York.
}
%\examples{}
\keyword{datasets}
