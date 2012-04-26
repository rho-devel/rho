% File nlme/man/Wafer.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Wafer}
\alias{Wafer}
\title{Modeling of Analog MOS Circuits}
\description{
    The \code{Wafer} data frame has 400 rows and 4 columns.
}
\format{
    This data frame contains the following columns:
    \describe{
        \item{Wafer}{
            a factor with levels
            \code{1} 
            \code{2} 
            \code{3} 
            \code{4} 
            \code{5} 
            \code{6} 
            \code{7} 
            \code{8} 
            \code{9} 
            \code{10} 
        }
        \item{Site}{
            a factor with levels
            \code{1} 
            \code{2} 
            \code{3} 
            \code{4} 
            \code{5} 
            \code{6} 
            \code{7} 
            \code{8} 
        }
        \item{voltage}{
            a numeric vector
        }
        \item{current}{
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
