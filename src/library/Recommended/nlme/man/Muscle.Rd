% File nlme/man/Muscle.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Muscle}
\alias{Muscle}
\title{Contraction of heart muscle sections}
\description{
    The \code{Muscle} data frame has 60 rows and 3 columns.
}
\format{
    This data frame contains the following columns:
    \describe{
        \item{Strip}{
            an ordered factor indicating the strip of muscle being measured.
        }
        \item{conc}{
            a numeric vector giving the concentration of CaCl2
        }
        \item{length}{
            a numeric vector giving the shortening of the heart muscle strip.
        }
    }
}
\details{
    Baumann and Waldvogel (1963) describe data on the shortening of heart
    muscle strips dipped in a \eqn{\mbox{CaCl}{}_2}{CaCl_2} solution.
    The muscle strips are taken from the left auricle of a rat's heart.}
\source{
    Baumann, F. and Waldvogel, F. (1963), La restitution pastsystolique
    de la contraction de l'oreillette gauche du rat. Effets de divers
    ions et de l'acetylcholine, \emph{Helvetica Physiologica Acta},
    \bold{21}.
}
%\examples{}
\keyword{datasets}
