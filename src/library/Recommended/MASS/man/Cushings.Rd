% file MASS/man/Cushings.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{Cushings}
\alias{Cushings}
\title{
  Diagnostic Tests on Patients with Cushing's Syndrome
}
\description{
  Cushing's syndrome is a hypertensive disorder associated with
  over-secretion of cortisol by the adrenal gland. The observations
  are urinary excretion rates of two steroid metabolites.
}
\usage{
Cushings
}
\format{
  The \code{Cushings} data frame has 27 rows and 3 columns:
  \describe{
    \item{\code{Tetrahydrocortisone}}{
      urinary excretion rate (mg/24hr) of Tetrahydrocortisone.
    }
    \item{\code{Pregnanetriol}}{
      urinary excretion rate (mg/24hr) of Pregnanetriol.
    }
    \item{\code{Type}}{
      underlying type of syndrome, coded \code{a} (adenoma) , \code{b}
      (bilateral hyperplasia), \code{c} (carcinoma) or \code{u} for unknown.
    }
  }
}
\source{
  J. Aitchison and I. R. Dunsmore (1975)
  \emph{Statistical Prediction Analysis.}
  Cambridge University Press, Tables 11.1--3.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\keyword{datasets}
