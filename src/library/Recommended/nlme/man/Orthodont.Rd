% File nlme/man/Orthodont.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Orthodont}
\alias{Orthodont}
\title{Growth curve data on an orthdontic measurement}
\description{
  The \code{Orthodont} data frame has 108 rows and 4 columns of the
  change in an orthdontic measurement over time for several young subjects.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{distance}{
      a numeric vector of distances from the pituitary to the
      pterygomaxillary fissure (mm).  These distances are measured
      on x-ray images of the skull.
    }
    \item{age}{
      a numeric vector of ages of the subject (yr).
    }
    \item{Subject}{
      an ordered factor indicating the subject on which the
      measurement was made.  The levels are labelled \code{M01}
      to \code{M16} for the males and \code{F01} to \code{F13} for
      the females.  The ordering is by increasing average distance
      within sex.
    }
    \item{Sex}{
      a factor with levels
      \code{Male} and
      \code{Female} 
    }
  }
}
\details{
  Investigators at the University of North Carolina Dental School
  followed the growth of 27 children (16 males, 11 females) from age 8
  until age 14.  Every two years they measured the distance between the
  pituitary and the pterygomaxillary fissure, two points that are easily
  identified on x-ray exposures of the side of the head.
}
\source{
  Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
    and S-PLUS}, Springer, New York.  (Appendix A.17)

  Potthoff, R. F. and Roy,  S. N. (1964), ``A generalized multivariate
  analysis of variance model useful especially for growth curve
  problems'', \emph{Biometrika}, \bold{51}, 313--326.
}
\examples{
formula(Orthodont)
plot(Orthodont)
}
\keyword{datasets}
