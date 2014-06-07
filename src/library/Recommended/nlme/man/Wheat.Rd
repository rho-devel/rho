% File nlme/man/Wheat.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Wheat}
\alias{Wheat}
\title{Yields by growing conditions}
\description{
    The \code{Wheat} data frame has 48 rows and 4 columns.
}
\format{
    This data frame contains the following columns:
    \describe{
        \item{Tray}{
            an ordered factor with levels
            \code{3} < \code{1} < \code{2} < \code{4} < \code{5} < \code{6} < \code{8} < \code{9} < \code{7} < \code{12} < \code{11} < \code{10}
        }
        \item{Moisture}{
            a numeric vector
        }
        \item{fertilizer}{
            a numeric vector
        }
        \item{DryMatter}{
            a numeric vector
        }
    }
}
\source{
    Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
        and S-PLUS}, Springer, New York.
}
%\examples{}
\keyword{datasets}
