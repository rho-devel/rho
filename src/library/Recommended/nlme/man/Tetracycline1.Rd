% File nlme/man/Tetracycline1.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Tetracycline1}
\alias{Tetracycline1}
\title{Pharmacokinetics of tetracycline}
\description{
    The \code{Tetracycline1} data frame has 40 rows and 4 columns.
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
            \code{5} < \code{3} < \code{2} < \code{4} < \code{1}
        }
        \item{Formulation}{
            a factor with levels
            \code{tetrachel} 
            \code{tetracyn} 
        }
    }
}
\source{
    Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
        and S-PLUS}, Springer, New York.
}
%\examples{}
\keyword{datasets}
