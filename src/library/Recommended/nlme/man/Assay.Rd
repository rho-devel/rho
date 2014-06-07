% File nlme/man/Assay.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Assay}
\alias{Assay}
\title{Bioassay on Cell Culture Plate}
\description{
    The \code{Assay} data frame has 60 rows and 4 columns.
}
\format{
    This data frame contains the following columns:
    \describe{
        \item{Block}{
            an ordered factor with levels
            \code{2} < \code{1} identifying the block where the wells
            are measured.
        }
        \item{sample}{
            a factor with levels \code{a} to \code{f} identifying the
            sample corresponding to the well.
        }
        \item{dilut}{
            a factor with levels
            \code{1} to \code{5}
            indicating the dilution applied to the well
        }
        \item{logDens}{
            a numeric vector of the log-optical density
        }
    }
}
\details{
    These data, courtesy of Rich Wolfe and David Lansky from Searle,
    Inc., come from a bioassay run on a 96-well cell culture plate.  The
    assay is performed using a split-block design.  The 8 rows on the
    plate are labeled A--H from top to bottom and the 12 columns on the
    plate are labeled 1--12 from left to right.  Only the central 60 wells
    of the plate are used for the bioassay (the intersection of rows B--G
    and columns 2--11).  There are two blocks in the design: Block 1
    contains columns 2--6 and Block 2 contains columns 7--11. Within each
    block, six samples are assigned randomly to rows and five (serial)
    dilutions are assigned randomly to columns. The response variable is
    the logarithm of the optical density. The cells are treated with a
    compound that they metabolize to produce the stain.  Only live cells
    can make the stain, so the optical density is a measure of the number
    of cells that are alive and healthy.}
\source{
   Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
     and S-PLUS}, Springer, New York. (Appendix A.2)
}
%\examples{}
\keyword{datasets}
