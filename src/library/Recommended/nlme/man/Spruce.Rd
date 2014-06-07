% File nlme/man/Spruce.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Spruce}
\alias{Spruce}
\title{Growth of Spruce Trees}
\description{
  The \code{Spruce} data frame has 1027 rows and 4 columns.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Tree}{
      a factor giving a unique identifier for each tree.
    }
    \item{days}{
      a numeric vector giving the number of days since the
      beginning of the experiment.
    }
    \item{logSize}{
      a numeric vector giving the logarithm of an estimate of the
      volume of the tree trunk.
    }
    \item{plot}{
      a factor identifying the plot in which the tree was grown.
    }
  }
}
\details{
  Diggle, Liang, and Zeger (1994, Example 1.3, page 5)
  describe data on the growth of spruce trees that have been exposed to
  an ozone-rich atmosphere or to a normal atmosphere.
}
\source{
  Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
    and S-PLUS}, Springer, New York.  (Appendix A.28)

  Diggle, Peter J., Liang, Kung-Yee and Zeger, Scott L. (1994),
  \emph{Analysis of longitudinal data}, Oxford University Press,
  Oxford.
}
%\examples{}
\keyword{datasets}
