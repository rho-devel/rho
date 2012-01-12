% File nlme/man/ergoStool.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{ergoStool}
\alias{ergoStool}
\title{Ergometrics experiment with stool types}
\description{
    The \code{ergoStool} data frame has 36 rows and 3 columns.
}
\format{
    This data frame contains the following columns:
    \describe{
        \item{effort}{
            a numeric vector giving the effort (Borg scale) required to
            arise from a stool.
        }
        \item{Type}{
            a factor with levels
            \code{T1}, 
            \code{T2}, 
            \code{T3}, and 
            \code{T4} giving the stool type.
        }
        \item{Subject}{
            an ordered factor giving a unique identifier for the subject
            in the experiment.
        }
    }
}
\details{
  Devore (2000) cites data from an article in \emph{Ergometrics} (1993,
  pp. 519-535) on ``The Effects of a Pneumatic Stool and a One-Legged
  Stool on Lower Limb Joint Load and Muscular Activity.''
}
\source{
    Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
        and S-PLUS}, Springer, New York.  (Appendix A.9)

    Devore, J. L. (2000), \emph{Probability and Statistics for
      Engineering and the Sciences (5th ed)}, Duxbury, Boston, MA.
}
\examples{
fm1 <-
   lme(effort ~ Type, data = ergoStool, random = ~ 1 | Subject)
anova( fm1 )
}
\keyword{datasets}
