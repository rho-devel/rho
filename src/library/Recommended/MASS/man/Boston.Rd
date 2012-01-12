% file MASS/man/Boston.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{Boston}
\alias{Boston}
\title{
  Housing Values in Suburbs of Boston
}
\description{
  The \code{Boston} data frame has 506 rows and 14 columns.
}
\usage{
Boston
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{crim}}{
      per capita crime rate by town.
    }
    \item{\code{zn}}{
      proportion of residential land zoned for lots over 25,000 sq.ft.
    }
    \item{\code{indus}}{
      proportion of non-retail business acres per town.
    }
    \item{\code{chas}}{
      Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
    }
    \item{\code{nox}}{
      nitrogen oxides concentration (parts per 10 million).
    }
    \item{\code{rm}}{
      average number of rooms per dwelling.
    }
    \item{\code{age}}{
      proportion of owner-occupied units built prior to 1940.
    }
    \item{\code{dis}}{
      weighted mean of distances to five Boston employment centres.
    }
    \item{\code{rad}}{
      index of accessibility to radial highways.
    }
    \item{\code{tax}}{
      full-value property-tax rate per \$10,000.
    }
    \item{\code{ptratio}}{
      pupil-teacher ratio by town.
    }
    \item{\code{black}}{
      \eqn{1000(Bk - 0.63)^2} where \eqn{Bk} is the proportion of blacks
      by town.
    }
    \item{\code{lstat}}{
      lower status of the population (percent).
    }
    \item{\code{medv}}{
      median value of owner-occupied homes in \$1000s.
    }
  }
}
\source{
  Harrison, D. and Rubinfeld, D.L. (1978)
  Hedonic prices and the demand for clean air.
  \emph{J. Environ. Economics and Management}
  \bold{5}, 81--102.

  Belsley D.A., Kuh, E.  and Welsch, R.E. (1980)
  \emph{Regression Diagnostics. Identifying Influential Data and Sources
    of Collinearity.}
  New York: Wiley.
}
\keyword{datasets}
