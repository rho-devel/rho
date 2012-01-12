% file MASS/man/birthwt.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{birthwt}
\alias{birthwt}
\title{
  Risk Factors Associated with Low Infant Birth Weight
}
\description{
  The \code{birthwt} data frame has 189 rows and 10 columns.
  The data were collected at Baystate Medical Center, Springfield, Mass
  during 1986.
}
\usage{
birthwt
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{low}}{indicator of birth weight less than 2.5 kg.}
    \item{\code{age}}{mother's age in years.}
    \item{\code{lwt}}{mother's weight in pounds at last menstrual period.}
    \item{\code{race}}{mother's race (\code{1} = white, \code{2} = black,
      \code{3} = other).}
    \item{\code{smoke}}{smoking status during pregnancy.}
    \item{\code{ptl}}{number of previous premature labours.}
    \item{\code{ht}}{history of hypertension.}
    \item{\code{ui}}{presence of uterine irritability.}
    \item{\code{ftv}}{number of physician visits during the first trimester.}
    \item{\code{bwt}}{birth weight in grams.}
  }
}
\source{
  Hosmer, D.W. and Lemeshow, S. (1989)
  \emph{Applied Logistic Regression.} New York: Wiley
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\examples{
attach(birthwt)
race <- factor(race, labels = c("white", "black", "other"))
ptd <- factor(ptl > 0)
ftv <- factor(ftv)
levels(ftv)[-(1:2)] <- "2+"
bwt <- data.frame(low = factor(low), age, lwt, race,
    smoke = (smoke > 0), ptd, ht = (ht > 0), ui = (ui > 0), ftv)
detach("birthwt")
options(contrasts = c("contr.treatment", "contr.poly"))
glm(low ~ ., binomial, bwt)
}
\keyword{datasets}
