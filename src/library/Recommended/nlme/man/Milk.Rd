% File nlme/man/Milk.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Milk}
\alias{Milk}
\title{Protein content of cows' milk}
\description{
    The \code{Milk} data frame has 1337 rows and 4 columns.
}
\format{
    This data frame contains the following columns:
    \describe{
        \item{protein}{
            a numeric vector giving the protein content of the milk.
        }
        \item{Time}{
            a numeric vector giving the time since calving (weeks).
        }
        \item{Cow}{
            an ordered factor giving a unique identifier for each cow.
        }
        \item{Diet}{
            a factor with levels
            \code{barley}, 
            \code{barley+lupins}, and 
            \code{lupins} identifying the diet for each cow.
        }
    }
}
\details{
    Diggle, Liang, and Zeger (1994) describe data on the protein content
    of cows' milk in the weeks following calving.  The cattle are grouped
    according to whether they are fed a diet with barley alone, with
    barley and lupins, or with lupins alone. 
}
\source{
     Diggle, Peter J., Liang, Kung-Yee and Zeger, Scott L. (1994),
     \emph{Analysis of longitudinal data}, Oxford University Press,
     Oxford.
}
%\examples{}
\keyword{datasets}
