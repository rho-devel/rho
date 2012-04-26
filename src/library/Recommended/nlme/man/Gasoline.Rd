% File nlme/man/Gasoline.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

\name{Gasoline}
\alias{Gasoline}
\title{Refinery yield of gasoline}
\description{
  The \code{Gasoline} data frame has 32 rows and 6 columns.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{yield}{
      a numeric vector giving the percentage of crude oil converted to
      gasoline after distillation and fractionation
    }
    \item{endpoint}{
        a numeric vector giving the temperature (degrees F) at which all
        the gasoline is vaporized
    }
    \item{Sample}{
      an ordered factor giving the inferred crude oil sample number
    }
    \item{API}{
      a numeric vector giving the crude oil gravity (degrees API)
    }
    \item{vapor}{
        a numeric vector giving the vapor pressure of the crude oil
        \eqn{(\mathrm{lbf}/\mathrm{in}^2)}{(lbf/in^2)}
    }
    \item{ASTM}{
      a numeric vector giving the crude oil 10\% point ASTM---the
      temperature at which 10\% of the crude oil has become vapor. 
    }
  }
}
\details{
  Prater (1955) provides data on crude oil properties and
  gasoline yields.  Atkinson (1985)
  uses these data to illustrate the use of diagnostics in multiple
  regression analysis.  Three of the covariates---\code{API},
  \code{vapor}, and \code{ASTM}---measure characteristics of the
  crude oil used to produce the gasoline.  The other covariate ---
  \code{endpoint}---is a characteristic of the refining process.
  Daniel and Wood (1980) notice that the covariates characterizing
  the crude oil occur in only ten distinct groups and conclude that the
  data represent responses measured on ten different crude oil samples.
}
\source{
  Prater, N. H. (1955), Estimate gasoline yields from crudes,
  \emph{Petroleum Refiner}, \bold{35} (5).

  Atkinson, A. C. (1985), \emph{Plots, Transformations, and
    Regression}, Oxford Press, New York.

  Daniel, C. and Wood, F. S. (1980), \emph{Fitting Equations to Data},
  Wiley, New York

  Venables, W. N. and Ripley, B. D. (1999) \emph{Modern Applied
    Statistics with S-PLUS (3rd ed)}, Springer, New York.
}
%\examples{}
\keyword{datasets}
