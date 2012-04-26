% File nlme/man/Cefamandole.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Cefamandole}
\alias{Cefamandole}
\title{Pharmacokinetics of Cefamandole}
\description{
  The \code{Cefamandole} data frame has 84 rows and 3 columns.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Subject}{
      a factor giving the subject from which the sample was drawn.
    }
    \item{Time}{
      a numeric vector giving the time at which the sample was drawn
      (minutes post-injection).
    }
    \item{conc}{
      a numeric vector giving the observed plasma concentration of
      cefamandole (mcg/ml).
    }
  }
}
\details{
  Davidian and Giltinan (1995, 1.1, p. 2) describe data
  obtained during a pilot study to investigate the pharmacokinetics of
  the drug cefamandole. Plasma concentrations of the drug were measured
  on six healthy volunteers at 14 time points following an intraveneous
  dose of 15 mg/kg body weight of cefamandole.
}
\source{
  Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
    and S-PLUS}, Springer, New York.  (Appendix A.4)

  Davidian, M. and Giltinan, D. M. (1995), \emph{Nonlinear Models for
    Repeated Measurement Data},  Chapman and Hall, London.
}
\examples{
plot(Cefamandole)
fm1 <- nlsList(SSbiexp, data = Cefamandole)
summary(fm1)
}
\keyword{datasets}
