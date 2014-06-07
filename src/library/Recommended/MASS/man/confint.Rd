% file MASS/man/confint.Rd
% copyright (C) 1999-2010 W. N. Venables and B. D. Ripley
%
\name{confint-MASS}
\alias{confint.glm}
\alias{confint.nls}
\alias{confint.profile.glm}
\alias{confint.profile.nls}
%\alias{profile.glm}
\title{
Confidence Intervals for Model Parameters
}
\description{
  Computes confidence intervals for one or more parameters in a fitted
  model.  Package \pkg{MASS} adds methods for \code{glm} and \code{nls} fits.
}
\usage{
\method{confint}{glm}(object, parm, level = 0.95, trace = FALSE, \dots)

\method{confint}{nls}(object, parm, level = 0.95, \dots)
}
\arguments{
  \item{object}{
    a fitted model object.  Methods currently exist for the classes
    \code{"glm"}, \code{"nls"} and for profile objects from these classes.
  }
  \item{parm}{
    a specification of which parameters are to be given confidence
    intervals, either a vector of numbers or a vector of names.  If
    missing, all parameters are considered.
  }
  \item{level}{
    the confidence level required.
  }
  \item{trace}{
    logical.  Should profiling be traced?
  }
  \item{\dots}{
    additional argument(s) for methods.
  }
}
\value{
  A matrix (or vector) with columns giving lower and upper confidence
  limits for each parameter.  These will be labelled as (1 - level)/2 and
  1 - (1 - level)/2 in \% (by default 2.5\% and 97.5\%).
}
\details{
  \code{\link[stats]{confint}} is a generic function in package \code{stats}.
  
  These \code{confint} methods call the appropriate profile method,
  then find the confidence intervals by interpolation in the profile
  traces.  If the profile object is already available it should be used
  as the main argument rather than the fitted model object itself.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
  \code{\link[stats]{confint}} (the generic and \code{"lm"} method),
  \code{\link{profile}}
}
\examples{
expn1 <- deriv(y ~ b0 + b1 * 2^(-x/th), c("b0", "b1", "th"),
               function(b0, b1, th, x) {})

wtloss.gr <- nls(Weight ~ expn1(b0, b1, th, Days),
   data = wtloss, start = c(b0=90, b1=95, th=120))

expn2 <- deriv(~b0 + b1*((w0 - b0)/b1)^(x/d0),
         c("b0","b1","d0"), function(b0, b1, d0, x, w0) {})

wtloss.init <- function(obj, w0) {
  p <- coef(obj)
  d0 <-  - log((w0 - p["b0"])/p["b1"])/log(2) * p["th"]
  c(p[c("b0", "b1")], d0 = as.vector(d0))
}

out <- NULL
w0s <- c(110, 100, 90)
for(w0 in w0s) {
    fm <- nls(Weight ~ expn2(b0, b1, d0, Days, w0),
              wtloss, start = wtloss.init(wtloss.gr, w0))
    out <- rbind(out, c(coef(fm)["d0"], confint(fm, "d0")))
  }
dimnames(out) <- list(paste(w0s, "kg:"),  c("d0", "low", "high"))
out

ldose <- rep(0:5, 2)
numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex <- factor(rep(c("M", "F"), c(6, 6)))
SF <- cbind(numdead, numalive = 20 - numdead)
budworm.lg0 <- glm(SF ~ sex + ldose - 1, family = binomial)
confint(budworm.lg0)
confint(budworm.lg0, "ldose")
}
\keyword{models}
