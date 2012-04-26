% file MASS/man/Cars93.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{Cars93}
\alias{Cars93}
\title{
  Data from 93 Cars on Sale in the USA in 1993
}
\description{
  The \code{Cars93} data frame has 93 rows and 27 columns.
}
\usage{
Cars93
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{Manufacturer}}{
      Manufacturer.
    }
    \item{\code{Model}}{
      Model.
    }
    \item{\code{Type}}{
      Type: a factor with levels \code{"Small"}, \code{"Sporty"},
      \code{"Compact"}, \code{"Midsize"}, \code{"Large"} and \code{"Van"}.
    }
    \item{\code{Min.Price}}{
      Minimum Price (in \$1,000): price for a basic version.
    }
    \item{\code{Price}}{
      Midrange Price (in \$1,000): average of \code{Min.Price} and
      \code{Max.Price}.
    }
    \item{\code{Max.Price}}{
      Maximum Price (in \$1,000): price for \dQuote{a premium version}.
    }
    \item{\code{MPG.city}}{
      City MPG (miles per US gallon by EPA rating).
    }
    \item{\code{MPG.highway}}{
      Highway MPG.
    }
    \item{\code{AirBags}}{
      Air Bags standard. Factor: none, driver only, or driver & passenger.
    }
    \item{\code{DriveTrain}}{
      Drive train type: rear wheel, front wheel or 4WD; (factor).
    }
    \item{\code{Cylinders}}{
      Number of cylinders (missing for Mazda RX-7, which has a rotary engine).
    }
    \item{\code{EngineSize}}{
      Engine size (litres).
    }
    \item{\code{Horsepower}}{
      Horsepower (maximum).
    }
    \item{\code{RPM}}{
      RPM (revs per minute at maximum horsepower).
    }
    \item{\code{Rev.per.mile}}{
      Engine revolutions per mile (in highest gear).
    }
    \item{\code{Man.trans.avail}}{
      Is a manual transmission version available? (yes or no, Factor).
    }
    \item{\code{Fuel.tank.capacity}}{
      Fuel tank capacity (US gallons).
    }
    \item{\code{Passengers}}{
      Passenger capacity (persons)
    }
    \item{\code{Length}}{
      Length (inches).
    }
    \item{\code{Wheelbase}}{
      Wheelbase (inches).
    }
    \item{\code{Width}}{
      Width (inches).
    }
    \item{\code{Turn.circle}}{
      U-turn space (feet).
    }
    \item{\code{Rear.seat.room}}{
      Rear seat room (inches) (missing for 2-seater vehicles).
    }
    \item{\code{Luggage.room}}{
      Luggage capacity (cubic feet) (missing for vans).
    }
    \item{\code{Weight}}{
      Weight (pounds).
    }
    \item{\code{Origin}}{
      Of non-USA or USA company origins? (factor).
    }
    \item{\code{Make}}{
      Combination of Manufacturer and Model (character).
    }
  }
}
\details{
  Cars were selected at random from among 1993 passenger car models that
  were listed in both the \emph{Consumer Reports} issue and the
  \emph{PACE Buying Guide}.  Pickup trucks and Sport/Utility vehicles were
  eliminated due to incomplete information in the \emph{Consumer Reports}
  source.  Duplicate models (e.g., Dodge Shadow and Plymouth Sundance)
  were listed at most once.

  Further description can be found in Lock (1993).
}
\source{
  Lock, R. H. (1993)
  1993 New Car Data.
  \emph{Journal of Statistics Education}
  \bold{1}(1).
  \url{http://www.amstat.org/publications/jse/v1n1/datasets.lock.html}.  
}
\references{
  Venables, W. N. and Ripley, B. D. (1999)
  \emph{Modern Applied Statistics with S-PLUS.} Third
  Edition. Springer.
}
\keyword{datasets}
