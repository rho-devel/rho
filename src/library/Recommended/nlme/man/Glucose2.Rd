% File nlme/man/Glucose2.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Glucose2}
\alias{Glucose2}
\title{Glucose Levels Following Alcohol Ingestion}
\description{
  The \code{Glucose2} data frame has 196 rows and 4 columns.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Subject}{
      a factor with levels
      \code{1} to \code{7} identifying the subject whose glucose
      level is measured. 
    }
    \item{Date}{
      a factor with levels
      \code{1} 
      \code{2}
      indicating the occasion in which the experiment was conducted.
    }
    \item{Time}{
      a numeric vector giving the time since alcohol ingestion (in min/10).
    }
    \item{glucose}{
      a numeric vector giving the blood glucose level (in mg/dl).
    }
  }
}
\details{
  Hand and Crowder (Table A.14, pp. 180-181, 1996) describe data on
  the blood glucose levels measured at 14 time points over 5 hours for 7
  volunteers who took alcohol at time 0. The same experiment was
  repeated on a second date with the same subjects but with a dietary
  additive used for all subjects.
}
\source{
  Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
    and S-PLUS}, Springer, New York.  (Appendix A.10)

  Hand, D. and Crowder, M. (1996), \emph{Practical Longitudinal Data
    Analysis}, Chapman and Hall, London.
}
%\examples{}
\keyword{datasets}
