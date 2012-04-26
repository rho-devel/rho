% file MASS/man/cement.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{cement}
\alias{cement}
\title{
  Heat Evolved by Setting Cements
}
\description{
  Experiment on the heat evolved in the setting of each of 13 cements.
}
\usage{
cement
}
\details{
  Thirteen samples of Portland cement were set. For each sample, the
  percentages of the four main chemical ingredients was accurately
  measured.  While the cement was setting the amount of heat evolved was
  also measured.
}
\format{
  \describe{
    \item{\code{x1, x2, x3, x4}}{
      Proportions (\%) of active ingredients.
    }
    \item{\code{y}}{
      heat evolved in cals/gm.
    }
  }
}
\source{
  Woods, H., Steinour, H.H. and Starke, H.R. (1932) Effect of composition of 
  Portland cement on heat evolved during hardening. 
  \emph{Industrial Engineering and Chemistry}, \bold{24}, 1207--1214.
}
\references{
  Hald, A. (1957) \emph{Statistical Theory with Engineering
    Applications.} Wiley, New York.
}
\examples{
lm(y ~ x1 + x2 + x3 + x4, cement)
}
\keyword{datasets}
