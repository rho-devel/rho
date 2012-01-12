% File nlme/man/Phenobarb.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Phenobarb}
\alias{Phenobarb}
\title{Phenobarbitol Kinetics}
\description{
  The \code{Phenobarb} data frame has 744 rows and 7 columns.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Subject}{
      an ordered factor identifying the infant.
    }
    \item{Wt}{
      a numeric vector giving the birth weight of the infant (kg).
    }
    \item{Apgar}{
      an ordered factor giving the 5-minute Apgar score for
      the infant.  This is an indication of health of the newborn
      infant.
    }
    \item{ApgarInd}{
      a factor indicating whether the 5-minute Apgar score is \code{< 5}
      or \code{>= 5}. 
    }
    \item{time}{
      a numeric vector giving the time when the sample is drawn or
      drug administered (hr).
    }
    \item{dose}{
      a numeric vector giving the dose of drug administered
      (\eqn{u}{\mu}g/kg).
    }
    \item{conc}{
      a numeric vector giving the phenobarbital concentration in
      the serum (\eqn{u}{\mu}g/L).
    }
  }
}
\details{
  Data from a pharmacokinetics study of phenobarbital in neonatal
  infants.  During the first few days of life the infants receive
  multiple doses of phenobarbital for prevention of seizures.  At
  irregular intervals blood samples are drawn and serum phenobarbital
  concentrations are determined.  The data
  were originally given in Grasela and Donn(1985) and are analyzed
  in Boeckmann, Sheiner and Beal (1994), in
  Davidian and Giltinan (1995), and in Littell et al. (1996).
}
\source{
  Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
    and S-PLUS}, Springer, New York.  (Appendix A.23)

  Davidian, M. and Giltinan, D. M. (1995), \emph{Nonlinear Models for
    Repeated Measurement Data},  Chapman and Hall, London.  (section 6.6)

  Grasela and Donn (1985), Neonatal population pharmacokinetics of
  phenobarbital derived from routine clinical data,
  \emph{Developmental Pharmacology and Therapeutics}, \bold{8},
  374-383.

  Boeckmann, A. J., Sheiner, L. B., and Beal, S. L. (1994),
  \emph{NONMEM Users Guide: Part V}, University of California, San
  Francisco.

  Littell, R. C., Milliken, G. A., Stroup, W. W. and Wolfinger,
  R. D. (1996), \emph{SAS System for Mixed Models}, SAS Institute,
  Cary, NC.
}
%\examples{}
\keyword{datasets}
