% file MASS/man/UScrime.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{UScrime}
\alias{UScrime}
\title{
  The Effect of Punishment Regimes on Crime Rates
}
\description{
  Criminologists are interested in the effect of punishment regimes on
  crime rates.  This has been studied using aggregate data on 47 states
  of the USA for 1960 given in this data frame.  The variables seem to
  have been re-scaled to convenient numbers.
}
\usage{
UScrime
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{M}}{
      percentage of males aged 14--24.
    }
    \item{\code{So}}{
      indicator variable for a Southern state.
    }
    \item{\code{Ed}}{
      mean years of schooling.
    }
    \item{\code{Po1}}{
      police expenditure in 1960.
    }
    \item{\code{Po2}}{
      police expenditure in 1959.
    }
    \item{\code{LF}}{
      labour force participation rate.
    }
    \item{\code{M.F}}{
      number of males per 1000 females.
    }
    \item{\code{Pop}}{
      state population.
    }
    \item{\code{NW}}{
      number of non-whites per 1000 people.
    }
    \item{\code{U1}}{
      unemployment rate of urban males 14--24.
    }
    \item{\code{U2}}{
      unemployment rate of urban males 35--39.
    }
    \item{\code{GDP}}{
      gross domestic product per head.
    }
    \item{\code{Ineq}}{
      income inequality.
    }
    \item{\code{Prob}}{
      probability of imprisonment.
    }
    \item{\code{Time}}{
      average time served in state prisons.
    }
    \item{\code{y}}{
      rate of crimes in a particular category per head of population.
    }
  }
}
\source{
  Ehrlich, I. (1973) Participation in illegitimate activities: a
  theoretical and empirical investigation.
  \emph{Journal of Political Economy}, \bold{81}, 521--565.

  Vandaele, W. (1978) Participation in illegitimate activities: Ehrlich
  revisited.  In \emph{Deterrence and Incapacitation},
  eds A. Blumstein, J. Cohen and D. Nagin, pp. 270--335.
  US National Academy of Sciences.
}
\references{
  Venables, W. N. and Ripley, B. D. (1999)
  \emph{Modern Applied Statistics with S-PLUS.} Third
  Edition. Springer.
}
\keyword{datasets}
