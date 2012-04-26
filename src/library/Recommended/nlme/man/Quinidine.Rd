% File nlme/man/Quinidine.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Quinidine}
\alias{Quinidine}
\title{Quinidine Kinetics}
\description{
  The \code{Quinidine} data frame has 1471 rows and 14 columns.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Subject}{
      a factor identifying the patient on whom the data were collected.
    }
    \item{time}{
      a numeric vector giving the time (hr) at which the drug was
      administered or the blood sample drawn.  This is measured
      from the time the patient entered the study.
    }
    \item{conc}{
      a numeric vector giving the serum quinidine concentration (mg/L).
    }
    \item{dose}{
      a numeric vector giving the dose of drug administered (mg).
      Although there were two different forms of quinidine
      administered, the doses were adjusted for differences in
      salt content by conversion to milligrams of quinidine base.
    }
    \item{interval}{
      a numeric vector giving the when the drug has been given at
      regular intervals for a sufficiently long period of time to
      assume steady state behavior, the interval is recorded.
    }
    \item{Age}{
      a numeric vector giving the age of the subject on entry to
      the study (yr).
    }
    \item{Height}{
      a numeric vector giving the height of the subject on entry
      to the study (in.).
    }
    \item{Weight}{
      a numeric vector giving the body weight of the subject (kg).
    }
    \item{Race}{
      a factor with levels
      \code{Caucasian}, 
      \code{Latin}, and
      \code{Black} identifying the race of the subject.
    }
    \item{Smoke}{
      a factor with levels
      \code{no} and
      \code{yes} giving smoking status at the time of the
      measurement.
    }
    \item{Ethanol}{
      a factor with levels
      \code{none}, 
      \code{current},
      \code{former} giving ethanol (alcohol) abuse status at the
      time of the measurement.
    }
    \item{Heart}{
      a factor with levels
      \code{No/Mild}, 
      \code{Moderate}, and 
      \code{Severe} indicating congestive heart failure for the subject.
    }
    \item{Creatinine}{
      an ordered factor with levels
      \code{< 50} < \code{>= 50} indicating the creatine clearance (mg/min).
    }
    \item{glyco}{
      a numeric vector giving the alpha-1 acid glycoprotein
      concentration (mg/dL).  Often measured at the same time as
      the quinidine concentration.
    }
  }
}
\details{
  Verme et al. (1992) analyze routine clinical data on
  patients receiving the drug quinidine as a treatment for cardiac
  arrythmia (atrial fibrillation of ventricular arrythmias).  All
  patients were receiving oral quinidine doses.  At irregular intervals
  blood samples were drawn and serum concentrations of quinidine were
  determined.  These data are
  analyzed in several publications, including Davidian and Giltinan (1995,
  section 9.3).
}
\source{
  Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
    and S-PLUS}, Springer, New York.  (Appendix A.25)

  Davidian, M. and Giltinan, D. M. (1995), \emph{Nonlinear Models for
    Repeated Measurement Data},  Chapman and Hall, London.

  Verme, C. N., Ludden,  T. M., Clementi, W. A. and Harris,
  S. C. (1992), Pharmacokinetics of quinidine in male patients: A
  population analysis, \emph{Clinical Pharmacokinetics}, \bold{22},
  468-480.
}
%\examples{}
\keyword{datasets}
