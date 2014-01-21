% File nlme/man/Oats.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Oats}
\alias{Oats}
\title{Split-plot Experiment on Varieties of Oats}
\description{
  The \code{Oats} data frame has 72 rows and 4 columns.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Block}{
      an ordered factor with levels
      \code{VI} < \code{V} < \code{III} < \code{IV} < \code{II} < \code{I}
    }
    \item{Variety}{
      a factor with levels
      \code{Golden Rain} 
      \code{Marvellous} 
      \code{Victory} 
    }
    \item{nitro}{
      a numeric vector
    }
    \item{yield}{
      a numeric vector
    }
  }
}
\details{
  These data have been introduced by Yates (1935) as an example of a
  split-plot design. The treatment structure used in the experiment was
  a \eqn{3 \times 4}{3 x 4} full factorial, with three varieties of oats
  and four concentrations of nitrogen. The experimental units were
  arranged into six blocks, each with three whole-plots subdivided into
  four subplots. The varieties of oats were assigned randomly to the
  whole-plots and the concentrations of nitrogen to the subplots.  All
  four concentrations of nitrogen were used on each whole-plot.
}
\source{
  Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
    and S-PLUS}, Springer, New York.  (Appendix A.15)

  Venables, W. N. and Ripley, B. D. (2002) \emph{Modern Applied
    Statistics with S. (4th ed)}, Springer, New York.
}
%\examples{}
\keyword{datasets}
