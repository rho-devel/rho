\name{bacteria}
\alias{bacteria}
\title{
  Presence of Bacteria after Drug Treatments
}
\description{
  Tests of the presence of the bacteria \emph{H. influenzae}
  in children with otitis media in the Northern Territory of Australia.
}
\usage{
bacteria
}
\format{
This data frame has 220 rows and the following columns:
  \describe{
    \item{y}{presence or absence: a factor with levels
      \code{n} and \code{y}.}
    \item{ap}{active/placebo: a factor with levels \code{a} and \code{p}.}
    \item{hilo}{hi/low compliance: a factor with levels \code{hi} amd
      \code{lo}.}
    \item{week}{numeric: week of test.}
    \item{ID}{subject ID: a factor.}
    \item{trt}{a factor with levels \code{placebo}, \code{drug} and
      \code{drug+}, a re-coding of \code{ap} and \code{hilo}.}
  }
}
\details{
  Dr A. Leach tested the effects of a drug on 50 children with a history of
  otitis media in the Northern Territory of Australia.  The children
  were randomized to the drug or the a placebo, and also to receive
  active encouragement to comply with taking the drug.

  The presence of \emph{H. influenzae} was checked at weeks 0, 2, 4, 6
  and 11: 30 of the checks were missing and are not included in this
  data frame.
}
\source{
  Menzies School of Health Research 1999--2000 Annual Report pp. 18--21
  (\url{http://www.menzies.edu.au/publications/anreps/MSHR00.pdf}).
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\examples{
contrasts(bacteria$trt) <- structure(contr.sdif(3),
     dimnames = list(NULL, c("drug", "encourage")))
## fixed effects analyses
summary(glm(y ~ trt * week, binomial, data = bacteria))
summary(glm(y ~ trt + week, binomial, data = bacteria))
summary(glm(y ~ trt + I(week > 2), binomial, data = bacteria))

# conditional random-effects analysis
library(survival)
bacteria$Time <- rep(1, nrow(bacteria))
coxph(Surv(Time, unclass(y)) ~ week + strata(ID),
      data = bacteria, method = "exact")
coxph(Surv(Time, unclass(y)) ~ factor(week) + strata(ID),
      data = bacteria, method = "exact")
coxph(Surv(Time, unclass(y)) ~ I(week > 2) + strata(ID),
      data = bacteria, method = "exact")

# PQL glmm analysis
library(nlme)
summary(glmmPQL(y ~ trt + I(week > 2), random = ~ 1 | ID,
                family = binomial, data = bacteria))
}
\keyword{datasets}
