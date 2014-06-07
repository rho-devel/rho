\name{VA}
\alias{VA}
\title{
  Veteran's Administration Lung Cancer Trial
}
\description{
  Veteran's Administration lung cancer trial from Kalbfleisch & Prentice.
}
\usage{
VA
}
\format{
  A data frame with columns:
  \describe{
    \item{\code{stime}}{
      survival or follow-up time in days.
    }
    \item{\code{status}}{
      dead or censored.
    }
    \item{\code{treat}}{
     treatment: standard or test.
   }
   \item{\code{age}}{
     patient's age in years.
   }
   \item{\code{Karn}}{
     Karnofsky score of patient's performance on a scale of 0 to 100.
   }
   \item{\code{diag.time}}{
     times since diagnosis in months at entry to trial.
   }
   \item{\code{cell}}{
     one of four cell types.
   }
   \item{\code{prior}}{
     prior therapy?
   }
 }
}
\source{
  Kalbfleisch, J.D. and Prentice R.L. (1980)
  \emph{The Statistical Analysis of Failure Time Data.}
  Wiley.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\keyword{datasets}
