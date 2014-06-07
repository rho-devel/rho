% File nlme/man/Oxide.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Oxide}
\alias{Oxide}
\title{Variability in Semiconductor Manufacturing}
\description{
  The \code{Oxide} data frame has 72 rows and 5 columns.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Source}{
      a factor with levels
      \code{1} and
      \code{2} 
    }
    \item{Lot}{
      a factor giving a unique identifier for each lot.
    }
    \item{Wafer}{
      a factor giving a unique identifier for each wafer within a lot.
    }
    \item{Site}{
      a factor with levels
      \code{1}, 
      \code{2}, and 
      \code{3} 
    }
    \item{Thickness}{
      a numeric vector giving the thickness of the oxide layer.
    }
  }
}
\details{
  These data are described in Littell et al. (1996, p. 155) as coming
  ``from a passive data collection study in the semiconductor industry
  where the objective is to estimate the variance components to
  determine the assignable causes of the observed variability.'' The
  observed response is the thickness of the oxide layer on silicon
  wafers, measured at three different sites of each of three wafers
  selected from each of eight lots sampled from the population of
  lots.
}
\source{
  Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
    and S-PLUS}, Springer, New York.  (Appendix A.20)

  Littell, R. C., Milliken, G. A., Stroup, W. W. and Wolfinger,
  R. D. (1996), \emph{SAS System for Mixed Models}, SAS Institute,
  Cary, NC.
}
%\examples{}
\keyword{datasets}
