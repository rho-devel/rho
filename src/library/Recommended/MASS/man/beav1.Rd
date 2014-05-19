% file MASS/man/beav1.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{beav1}
\alias{beav1}
\title{
  Body Temperature Series of Beaver 1
}
\description{
  Reynolds (1994) describes a small part of a study of the long-term
  temperature dynamics of beaver \emph{Castor canadensis} in
  north-central Wisconsin.  Body temperature was measured by telemetry
  every 10 minutes for four females, but data from a one period of less
  than a day for each of two animals is used there.
}
\usage{
beav1
}
\format{
  The \code{beav1} data frame has 114 rows and 4 columns.
  This data frame contains the following columns:
  \describe{
    \item{\code{day}}{
      Day of observation (in days since the beginning of 1990),
      December 12--13.
    }
    \item{\code{time}}{
      Time of observation, in the form \code{0330} for 3.30am.
    }
    \item{\code{temp}}{
      Measured body temperature in degrees Celsius.
    }
    \item{\code{activ}}{
      Indicator of activity outside the retreat.
    }
  }
}
\note{
  The observation at 22:20 is missing.
}
\source{
  P. S. Reynolds (1994) Time-series analyses of beaver body temperatures.
  Chapter 11 of
  Lange, N., Ryan, L., Billard, L., Brillinger, D., Conquest, L.
  and Greenhouse, J. eds (1994) \emph{Case Studies in Biometry.} New
  York: John Wiley and Sons.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{beav2}}
}
\examples{
beav1 <- within(beav1,
               hours <- 24*(day-346) + trunc(time/100) + (time\%\%100)/60)
plot(beav1$hours, beav1$temp, type="l", xlab="time",
   ylab="temperature", main="Beaver 1")
usr <- par("usr"); usr[3:4] <- c(-0.2, 8); par(usr=usr)
lines(beav1$hours, beav1$activ, type="s", lty=2)
temp <- ts(c(beav1$temp[1:82], NA, beav1$temp[83:114]),
           start = 9.5, frequency = 6)
activ <- ts(c(beav1$activ[1:82], NA, beav1$activ[83:114]),
            start = 9.5, frequency = 6)

acf(temp[1:53])
acf(temp[1:53], type = "partial")
ar(temp[1:53])
act <- c(rep(0, 10), activ)
X <- cbind(1, act = act[11:125], act1 = act[10:124],
          act2 = act[9:123], act3 = act[8:122])
alpha <- 0.80
stemp <- as.vector(temp - alpha*lag(temp, -1))
sX <- X[-1, ] - alpha * X[-115,]
beav1.ls <- lm(stemp ~ -1 + sX, na.action = na.omit)
summary(beav1.ls, cor = FALSE)
rm(temp, activ)
}
\keyword{datasets}
