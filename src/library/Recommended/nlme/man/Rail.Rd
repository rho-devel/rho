% File nlme/man/Rail.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Rail}
\alias{Rail}
\title{Evaluation of Stress in Railway Rails}
\description{
    The \code{Rail} data frame has 18 rows and 2 columns.
}
\format{
    This data frame contains the following columns:
    \describe{
        \item{Rail}{
            an ordered factor identifying the rail on which the
            measurement was made.
        }
        \item{travel}{
          a numeric vector giving the travel time for ultrasonic
          head-waves in the rail (nanoseconds).  The value given is the
          original travel time minus 36,100 nanoseconds.
        }
    }
}
\details{
  Devore (2000, Example 10.10, p. 427) cites data from an article in
  \emph{Materials Evaluation} on ``a study of travel time for a certain
  type of wave that results from longitudinal stress of rails used for
  railroad track.''
}
\source{
    Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
        and S-PLUS}, Springer, New York.  (Appendix A.26)

    Devore, J. L. (2000), \emph{Probability and Statistics for
      Engineering and the Sciences (5th ed)}, Duxbury, Boston, MA.
}
%\examples{}
\keyword{datasets}
