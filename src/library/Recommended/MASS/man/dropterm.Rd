% file MASS/man/dropterm.Rd
% copyright (C) 1998-9 W. N. Venables and B. D. Ripley
%
\name{dropterm}
\alias{dropterm}
\alias{dropterm.default}
\alias{dropterm.glm}
\alias{dropterm.lm}
%\alias{dropterm.mlm}
%\alias{dropterm.negbin}
%\alias{dropterm.survreg}
\title{
Try All One-Term Deletions from a Model
}
\description{
Try fitting all models that differ from the current model by dropping a
single term, maintaining marginality.


This function is generic; there exist methods for classes \code{lm} and
\code{glm} and the default method will work for many other classes.
}
\usage{
dropterm (object, \dots)

\method{dropterm}{default}(object, scope, scale = 0, test = c("none", "Chisq"),
         k = 2, sorted = FALSE, trace = FALSE, \dots)

\method{dropterm}{lm}(object, scope, scale = 0, test = c("none", "Chisq", "F"),
         k = 2, sorted = FALSE, \dots)

\method{dropterm}{glm}(object, scope, scale = 0, test = c("none", "Chisq", "F"),
         k = 2, sorted = FALSE, trace = FALSE, \dots)
}
\arguments{
\item{object}{
A object fitted by some model-fitting function.
}
\item{scope}{
a formula giving terms which might be dropped. By default, the
model formula. Only terms that can be dropped and maintain marginality
are actually tried.
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
Only \code{k = 2} gives the genuine AIC: \code{k = log(n)} is sometimes
referred to as BIC or SBC.
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
\code{\link{addterm}}, \code{\link{stepAIC}}
}
\examples{
quine.hi <- aov(log(Days + 2.5) ~ .^4, quine)
quine.nxt <- update(quine.hi, . ~ . - Eth:Sex:Age:Lrn)
dropterm(quine.nxt, test=  "F")
quine.stp <- stepAIC(quine.nxt,
    scope = list(upper = ~Eth*Sex*Age*Lrn, lower = ~1),
    trace = FALSE)
dropterm(quine.stp, test = "F")
quine.3 <- update(quine.stp, . ~ . - Eth:Age:Lrn)
dropterm(quine.3, test = "F")
quine.4 <- update(quine.3, . ~ . - Eth:Age)
dropterm(quine.4, test = "F")
quine.5 <- update(quine.4, . ~ . - Age:Lrn)
dropterm(quine.5, test = "F")

house.glm0 <- glm(Freq ~ Infl*Type*Cont + Sat, family=poisson,
                   data = housing)
house.glm1 <- update(house.glm0, . ~ . + Sat*(Infl+Type+Cont))
dropterm(house.glm1, test = "Chisq")
}
\keyword{models}
