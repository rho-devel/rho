% File nlme/man/MathAchSchool.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{MathAchSchool}
\alias{MathAchSchool}
\title{School demographic data for MathAchieve}
\description{
    The \code{MathAchSchool} data frame has 160 rows and 7 columns.
}
\format{
    This data frame contains the following columns:
    \describe{
        \item{School}{
            a factor giving the school on which the measurement is made.
        }
        \item{Size}{
            a numeric vector giving the number of students in the school
        }
        \item{Sector}{
            a factor with levels
            \code{Public} 
            \code{Catholic} 
        }
        \item{PRACAD}{
            a numeric vector giving the percentage of students on the
            academic track
        }
        \item{DISCLIM}{
            a numeric vector measuring the discrimination climate
        }
        \item{HIMINTY}{
            a factor with levels
            \code{0} 
            \code{1} 
        }
        \item{MEANSES}{
            a numeric vector giving the mean SES score.
        }
    }
}
\details{
  These variables give the school-level demographic data to accompany
  the \code{MathAchieve} data.
}
%\source{}
%\examples{}
\keyword{datasets}
