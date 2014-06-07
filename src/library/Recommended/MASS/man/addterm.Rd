% file MASS/man/addterm.Rd
% copyright (C) 1998-9 W. N. Venables and B. D. Ripley
%
\name{addterm}
\alias{addterm}
\alias{addterm.default}
\alias{addterm.glm}
\alias{addterm.lm}
%\alias{addterm.mlm}
%\alias{addterm.negbin}
%\alias{addterm.survreg}
\title{
Try All One-Term Additions to a Model
}
\description{
Try fitting all models that differ from the current model by adding a
single term from those supplied, maintaining marginality.


This function is generic; there exist methods for classes \code{lm} and
\code{glm} and the default method will work for many other classes.
}
\usage{
addterm(object, \dots)

\method{addterm}{default}(object, scope, scale = 0, test = c("none", "Chisq"),
        k = 2, sorted = FALSE, trace = FALSE, \dots)
\method{addterm}{lm}(object, scope, scale = 0, test = c("none", "Chisq", "F"),
        k = 2, sorted = FALSE, \dots)
\method{addterm}{glm}(object, scope, scale = 0, test = c("none", "Chisq", "F"),
        k = 2, sorted = FALSE, trace = FALSE, \dots)
}
\arguments{
\item{object}{
An object fitted by some model-fitting function.
}
\item{scope}{
a formula specifying a maximal model which should include the current
one. All additional terms in the maximal model with all marginal terms
in the original model are tried.
}
\item{scale}{
used in the definition of the AIC statistic for selecting the models,
currently only for \code{lm}, \code{aov} and \code{glm} models. Specifying \code{scale}
asserts that the residual standard error or dispersion is known.
}
\item{test}{
should the results include a test statistic relative to the original
model?  The F test is only appropriate for \code{lm} and \code{aov} models,
and perhaps for some over-dispersed \code{glm} models. The
Chisq test can be an exact test (\code{lm} models with known scale) or a
likelihood-ratio test depending on the method.
}
\item{k}{
the multiple of the number of degrees of freedom used for the penalty.
Only \code{k=2} gives the genuine AIC: \code{k = log(n)} is sometimes referred
to as BIC or SBC.
}
\item{sorted}{
should the results be sorted on the value of AIC?
}
\item{trace}{
if \code{TRUE} additional information may be given on the fits as they are tried.
}
\item{\dots}{
arguments passed to or from other methods.
}}
\value{
A table of class \code{"anova"} containing at least columns for the change
in degrees of freedom and AIC (or Cp) for the models. Some methods
will give further information, for example sums of squares, deviances,
log-likelihoods and test statistics.
}
\details{
The definition of AIC is only up to an additive constant: when
appropriate (\code{lm} models with specified scale) the constant is taken
to be that used in Mallows' Cp statistic and the results are labelled
accordingly.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{dropterm}}, \code{\link{stepAIC}}
}
\examples{
quine.hi <- aov(log(Days + 2.5) ~ .^4, quine)
quine.lo <- aov(log(Days+2.5) ~ 1, quine)
addterm(quine.lo, quine.hi, test="F")

house.glm0 <- glm(Freq ~ Infl*Type*Cont + Sat, family=poisson,
                   data=housing)
addterm(house.glm0, ~. + Sat:(Infl+Type+Cont), test="Chisq")
house.glm1 <- update(house.glm0, . ~ . + Sat*(Infl+Type+Cont))
addterm(house.glm1, ~. + Sat:(Infl+Type+Cont)^2, test = "Chisq")
}
\keyword{models}
