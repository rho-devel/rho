% File nlme/man/Fatigue.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Fatigue}
\alias{Fatigue}
\title{Cracks caused by metal fatigue}
\description{
  The \code{Fatigue} data frame has 262 rows and 3 columns.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Path}{
      an ordered factor with levels
      \code{1} < \code{2} < \code{3} < \code{4} < \code{5} <
      \code{6} < \code{7} < \code{8} < \code{9} < \code{10} <
      \code{11} < \code{12} < \code{13} < \code{14} < \code{15} <
      \code{16} < \code{17} < \code{18} < \code{19} < \code{20} <
      \code{21} giving the test path (or test unit) number.  The
      order is in terms of increasing failure time or decreasing
      terminal crack length.
    }
    \item{cycles}{
      number of test cycles at which the measurement is made
      (millions of cycles).
    }
    \item{relLength}{
      relative crack length (dimensionless).
    }
  }
}
\details{
  These data are given in Lu and Meeker (1993) where they state
  ``We obtained the data in Table 1 visually from figure 4.5.2 on page 242
  of Bogdanoff and Kozin (1985).''
  The data represent the growth of cracks in metal for 21 test units.
  An initial notch of length 0.90 inches was made on each unit which then
  was subjected to several thousand test cycles.  After every 10,000
  test cycles the crack length was measured.  Testing was stopped if the
  crack length exceeded 1.60 inches, defined as a failure, or at 120,000
  cycles.
}
\source{
  Lu, C. Joseph , and Meeker, William Q. (1993), Using degradation
  measures to estimate a time-to-failure distribution,
  \emph{Technometrics}, \bold{35}, 161-174 
}
%\examples{}
\keyword{datasets}
