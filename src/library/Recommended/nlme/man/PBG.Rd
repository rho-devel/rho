% File nlme/man/PBG.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{PBG}
\alias{PBG}
\title{Effect of Phenylbiguanide on Blood Pressure}
\description{
    The \code{PBG} data frame has 60 rows and 5 columns.
}
\format{
    This data frame contains the following columns:
    \describe{
        \item{deltaBP}{
            a numeric vector
        }
        \item{dose}{
            a numeric vector
        }
        \item{Run}{
            an ordered factor with levels
            \code{T5} < \code{T4} < \code{T3} < \code{T2} < \code{T1} < \code{P5} < \code{P3} < \code{P2} < \code{P4} < \code{P1}
        }
        \item{Treatment}{
            a factor with levels
            \code{MDL 72222} 
            \code{Placebo} 
        }
        \item{Rabbit}{
            an ordered factor with levels
            \code{5} < \code{3} < \code{2} < \code{4} < \code{1}
        }
    }
}
\details{
Data on an experiment to examine the effect of a antagonist MDL 72222
on the change in blood pressure experienced with increasing dosage of
phenylbiguanide are described in Ludbrook (1994) and analyzed
in Venables and Ripley (1999, section 8.8).  Each of five rabbits was exposed to
increasing doses of phenylbiguanide after having either a placebo or
the HD5-antagonist MDL 72222 administered.
}
\source{
    Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
      and S-PLUS}, Springer, New York.  (Appendix A.21)

    Venables, W. N. and Ripley, B. D. (1999) \emph{Modern Applied
      Statistics with S-PLUS (3rd ed)}, Springer, New York.

    Ludbrook, J. (1994), Repeated measurements and multiple comparisons
    in cardiovascular research, \emph{Cardiovascular Research},
    \bold{28}, 303-311.
}
%\examples{}
\keyword{datasets}
