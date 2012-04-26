% File nlme/man/BodyWeight.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{BodyWeight}
\alias{BodyWeight}
\title{Rat weight over time for different diets}
\description{
    The \code{BodyWeight} data frame has 176 rows and 4 columns.
}
\format{
    This data frame contains the following columns:
    \describe{
        \item{weight}{
            a numeric vector giving the body weight of the rat (grams).
        }
        \item{Time}{
            a numeric vector giving the time at which the measurement is
            made (days).
        }
        \item{Rat}{
            an ordered factor with levels \code{2} < \code{3} < \code{4}
            < \code{1} < \code{8} < \code{5} < \code{6} < \code{7} <
            \code{11} < \code{9} < \code{10} < \code{12} < \code{13} <
            \code{15} < \code{14} < \code{16} identifying the rat whose
            weight is measured.
        }
        \item{Diet}{
            a factor with levels
            \code{1} to \code{3} indicating the diet that the rat receives.
        }
    }
}
\details{
    Hand and Crowder (1996) describe data on the body weights of rats
    measured over 64 days.  These data also appear in Table 2.4 of
    Crowder and Hand (1990).  The body weights of the rats (in grams)
    are measured on day 1 and every seven days thereafter until day 64,
    with an extra measurement on day 44. The experiment started several
    weeks before ``day 1.''  There are three groups of rats, each on a
    different diet.
}
\source{
   Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
       and S-PLUS}, Springer, New York. (Appendix A.3)

   Crowder, M. and Hand, D. (1990), \emph{Analysis of Repeated
       Measures}, Chapman and Hall, London.

   Hand, D. and Crowder, M. (1996), \emph{Practical Longitudinal Data
       Analysis}, Chapman and Hall, London.
}
%\examples{}
\keyword{datasets}
