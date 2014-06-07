% File nlme/man/Ovary.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Ovary}
\alias{Ovary}
\title{Counts of Ovarian Follicles}
\description{
  The \code{Ovary} data frame has 308 rows and 3 columns.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Mare}{
      an ordered factor indicating the mare on which the
      measurement is made.
    }
    \item{Time}{
      time in the estrus cycle.  The data were recorded daily from 3
      days before ovulation until 3 days after the next ovulation. The
      measurement times for each mare are scaled so that the ovulations for
      each mare occur at times 0 and 1.
    }
    \item{follicles}{
      the number of ovarian follicles greater than 10 mm in diameter.
    }
  }
}
\details{
  Pierson and Ginther (1987) report on a study of the number of large
  ovarian follicles detected in different mares at several times in
  their estrus cycles.
}
\source{
  Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
    and S-PLUS}, Springer, New York.  (Appendix A.18)

  Pierson, R. A. and Ginther, O. J. (1987), Follicular population
  dynamics during the estrus cycle of the mare, \emph{Animal
    Reproduction Science}, \bold{14}, 219-231.
}
%\examples{}
\keyword{datasets}
