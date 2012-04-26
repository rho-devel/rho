% File nlme/man/quinModel.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{quinModel}
\alias{quinModel}
\title{Model function for the Quinidine data}
\description{
  A model function for a model used with the \code{Quinidine} data.
  This function calls compiled C code.
}
\usage{
quinModel(Subject, time, conc, dose, interval, lV, lKa, lCl)
}
\arguments{
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
  \item{lV}{numeric.  A vector of values of the natural log of the
    effective volume of distribution according to \code{Subject} and
    \code{time}.}
  \item{lKa}{numeric.  A vector of values of the natural log of the
    absorption rate constant according to \code{Subject} and \code{time}.}
  \item{lCl}{numeric.  A vector of values of the natural log of the
    clearance parameter according to \code{Subject} and \code{time}.}
}
\details{
  See the details section of \code{\link{Quinidine}} for a description
  of the model function that \code{quinModel} evaluates.
}
\value{
  a numeric vector of predicted quinidine concentrations.
}
\references{
    Pinheiro, J. C. and Bates, D. M. (2000) \emph{Mixed-effects Models in
        S and S-PLUS}, Springer. (section 8.2)
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}
}
\keyword{models}
