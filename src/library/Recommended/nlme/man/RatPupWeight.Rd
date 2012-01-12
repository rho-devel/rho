% File nlme/man/RatPupWeight.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{RatPupWeight}
\alias{RatPupWeight}
\title{The weight of rat pups}
\description{
    The \code{RatPupWeight} data frame has 322 rows and 5 columns.
}
\format{
    This data frame contains the following columns:
    \describe{
        \item{weight}{
            a numeric vector
        }
        \item{sex}{
            a factor with levels
            \code{Male} 
            \code{Female} 
        }
        \item{Litter}{
            an ordered factor with levels
            \code{9} < \code{8} < \code{7} < \code{4} < \code{2} < \code{10} < \code{1} < \code{3} < \code{5} < \code{6} < \code{21} < \code{22} < \code{24} < \code{27} < \code{26} < \code{25} < \code{23} < \code{17} < \code{11} < \code{14} < \code{13} < \code{15} < \code{16} < \code{20} < \code{19} < \code{18} < \code{12}
        }
        \item{Lsize}{
            a numeric vector
        }
        \item{Treatment}{
            an ordered factor with levels
            \code{Control} < \code{Low} < \code{High}
        }
    }
}
\source{
    Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
        and S-PLUS}, Springer, New York.
}
%\examples{}
\keyword{datasets}
