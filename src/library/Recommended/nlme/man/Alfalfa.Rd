% File nlme/man/Alfalfa.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Alfalfa}
\alias{Alfalfa}
\title{Split-Plot Experiment on Varieties of Alfalfa}
\description{
  The \code{Alfalfa} data frame has 72 rows and 4 columns.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Variety}{
      a factor with levels
      \code{Cossack},
      \code{Ladak}, and 
      \code{Ranger} 
    }
    \item{Date}{
      a factor with levels
      \code{None} 
      \code{S1} 
      \code{S20} 
      \code{O7} 
    }
    \item{Block}{
      a factor with levels
      \code{1} 
      \code{2} 
      \code{3} 
      \code{4} 
      \code{5} 
      \code{6} 
    }
    \item{Yield}{
      a numeric vector
    }
  }
}
\details{
  These data are described in Snedecor and Cochran (1980) as
  an example of a split-plot design. The treatment structure used in the
  experiment was a 3\eqn{ x }{\times}4 full factorial, with three varieties of
  alfalfa and four dates of third cutting in 1943. The experimental
  units were arranged into six blocks, each subdivided into four plots.
  The varieties of alfalfa (\emph{Cossac}, \emph{Ladak}, and
  \emph{Ranger}) were assigned randomly to the blocks and the dates of
  third cutting (\emph{None}, \emph{S1}---September 1,
  \emph{S20}---September 20, and \emph{O7}---October 7) were randomly
  assigned to the plots.  All four dates were used on each block.
}
\source{
  Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
    and S-PLUS}, Springer, New York.  (Appendix A.1)

  Snedecor, G. W. and Cochran, W. G. (1980), \emph{Statistical Methods (7th
    ed)}, Iowa State University Press, Ames, IA
}
%\examples{}
\keyword{datasets}
