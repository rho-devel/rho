% File nlme/man/Pixel.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Pixel}
\alias{Pixel}
\title{X-ray pixel intensities over time}
\description{
    The \code{Pixel} data frame has 102 rows and 4 columns of data on the
    pixel intensities of CT scans of dogs over time
}
\format{
    This data frame contains the following columns:
    \describe{
        \item{Dog}{
            a factor with levels \code{1} to \code{10} designating the dog
            on which the scan was made
        }
        \item{Side}{
            a factor with levels \code{L} and \code{R} designating the side
            of the dog being scanned
        }
        \item{day}{
            a numeric vector giving the day post injection of the contrast
            on which the scan was made
        }
        \item{pixel}{
            a numeric vector of pixel intensities
        }
    }
}
\source{
    Pinheiro, J. C. and Bates, D. M. (2000) \emph{Mixed-effects Models in
        S and S-PLUS}, Springer.
}
\examples{
fm1 <- lme(pixel ~ day + I(day^2), data = Pixel,
           random = list(Dog = ~ day, Side = ~ 1))
summary(fm1)
VarCorr(fm1)
}
\keyword{datasets}
