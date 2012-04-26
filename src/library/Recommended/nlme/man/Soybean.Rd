% File nlme/man/Soybean.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Soybean}
\alias{Soybean}
\title{Growth of soybean plants}
\description{
  The \code{Soybean} data frame has 412 rows and 5 columns.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Plot}{
      a factor giving a unique identifier for each plot.
    }
    \item{Variety}{
      a factor indicating the variety; Forrest (F) or Plant
      Introduction \#416937 (P).
    }
    \item{Year}{
      a factor indicating the year the plot was planted.
    }
    \item{Time}{
      a numeric vector giving the time the sample was taken (days
      after planting).
    }
    \item{weight}{
      a numeric vector giving the average leaf weight per plant (g).
    }
  }
}
\details{
  These data are described in
  Davidian and Giltinan (1995, 1.1.3, p.7) as ``Data from an
  experiment to compare growth patterns of two genotypes of soybeans:
  Plant Introduction \#416937 (P), an experimental strain, and Forrest
  (F), a commercial variety.''
}
\source{
  Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
    and S-PLUS}, Springer, New York.  (Appendix A.27)

  Davidian, M. and Giltinan, D. M. (1995), \emph{Nonlinear Models for
    Repeated Measurement Data},  Chapman and Hall, London.
}
\examples{
summary(fm1 <- nlsList(SSlogis, data = Soybean))
}
\keyword{datasets}
