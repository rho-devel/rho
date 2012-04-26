% File nlme/man/Machines.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Machines}
\alias{Machines}
\title{Productivity Scores for Machines and Workers}
\description{
  The \code{Machines} data frame has 54 rows and 3 columns.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Worker}{
      an ordered factor giving the unique identifier for the worker.
    }
    \item{Machine}{
      a factor with levels
      \code{A}, 
      \code{B}, and 
      \code{C} identifying the machine brand.
    }
    \item{score}{
      a productivity score.
    }
  }
}
\details{
  Data on an experiment to compare three brands of machines used in an
  industrial process are presented in Milliken and Johnson (p. 285,
  1992). Six workers were chosen randomly among the
  employees of a factory to operate each machine three times. The
  response is an overall productivity score taking into account the
  number and quality of components produced.}
\source{
  Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models
    in S and S-PLUS}, Springer, New York.  (Appendix A.14)

  Milliken, G. A. and Johnson, D. E. (1992), \emph{Analysis of Messy Data,
    Volume I: Designed Experiments}, Chapman and Hall, London.
}
%\examples{}
\keyword{datasets}
