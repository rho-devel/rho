\name{epil}
\alias{epil}
\title{
  Seizure Counts for Epileptics
}
\description{
  Thall and Vail (1990) give a data set on two-week seizure counts for
  59 epileptics.  The number of seizures was recorded for a baseline
  period of 8 weeks, and then patients were randomly assigned to a
  treatment group or a control group.  Counts were then recorded for
  four successive two-week periods. The subject's age is the only
  covariate.
}
\usage{
epil
}
\format{
  This data frame has 236 rows and the following 9 columns:
  \describe{
    \item{\code{y}}{
      the count for the 2-week period.
    }
    \item{\code{trt}}{
      treatment, \code{"placebo"} or \code{"progabide"}.
    }
    \item{\code{base}}{
      the counts in the baseline 8-week period.
    }
    \item{\code{age}}{
      subject's age, in years.
    }
    \item{\code{V4}}{
      \code{0/1} indicator variable of period 4.
    }
    \item{\code{subject}}{
      subject number, 1 to 59.
    }
    \item{\code{period}}{
      period, 1 to 4.
    }
    \item{\code{lbase}}{
      log-counts for the baseline period, centred to have zero mean.
    }
    \item{\code{lage}}{
      log-ages, centred to have zero mean.
    }
  }
}
\source{
  Thall, P. F. and Vail, S. C. (1990)
  Some covariance models for longitudinal count data with over-dispersion.
  \emph{Biometrics} \bold{46}, 657--671.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth Edition. Springer.
}
\examples{
summary(glm(y ~ lbase*trt + lage + V4, family = poisson,
            data = epil), cor = FALSE)
epil2 <- epil[epil$period == 1, ]
epil2["period"] <- rep(0, 59); epil2["y"] <- epil2["base"]
epil["time"] <- 1; epil2["time"] <- 4
epil2 <- rbind(epil, epil2)
epil2$pred <- unclass(epil2$trt) * (epil2$period > 0)
epil2$subject <- factor(epil2$subject)
epil3 <- aggregate(epil2, list(epil2$subject, epil2$period > 0),
   function(x) if(is.numeric(x)) sum(x) else x[1])
epil3$pred <- factor(epil3$pred,
   labels = c("base", "placebo", "drug"))

contrasts(epil3$pred) <- structure(contr.sdif(3),
    dimnames = list(NULL, c("placebo-base", "drug-placebo")))
summary(glm(y ~ pred + factor(subject) + offset(log(time)),
            family = poisson, data = epil3), cor = FALSE)

summary(glmmPQL(y ~ lbase*trt + lage + V4,
                random = ~ 1 | subject,
                family = poisson, data = epil))
summary(glmmPQL(y ~ pred, random = ~1 | subject,
                family = poisson, data = epil3))
}
\keyword{datasets}
