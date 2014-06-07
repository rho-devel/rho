% File nlme/man/IGF.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{IGF}
\alias{IGF}
\title{Radioimmunoassay of IGF-I Protein}
\description{
    The \code{IGF} data frame has 237 rows and 3 columns.
}
\format{
    This data frame contains the following columns:
    \describe{
        \item{Lot}{
            an ordered factor giving the radioactive tracer lot.
        }
        \item{age}{
            a numeric vector giving the age (in days) of the radioactive
            tracer.
        }
        \item{conc}{
            a numeric vector giving the estimated concentration of IGF-I
            protein (ng/ml)
        }
    }
}
\details{
    Davidian and Giltinan (1995) describe data obtained during quality
    control radioimmunoassays for ten different lots of radioactive
    tracer used to calibrate the Insulin-like Growth Factor (IGF-I)
    protein concentration measurements. 
}
\source{
    Davidian, M. and Giltinan, D. M. (1995), \emph{Nonlinear Models for
        Repeated Measurement Data},  Chapman and Hall, London.
    
    Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
        and S-PLUS}, Springer, New York.  (Appendix A.11)
}
%\examples{}
\keyword{datasets}
