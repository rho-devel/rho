% File nlme/man/anova.gls.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: anova.gls.Rd,v 1.6.2.1 2003/04/17 22:28:45 bates Exp $
\name{anova.gls}
\title{Compare Likelihoods of Fitted Objects}
\usage{
\method{anova}{gls}(object, \dots, test, type, adjustSigma, Terms, L, verbose)
}
\alias{anova.gls}
\arguments{
 \item{object}{a fitted model object inheriting from class \code{gls},
   representing a generalized least squares fit.}
 \item{\dots}{other optional fitted model objects inheriting from
   classes \code{gls}, \code{gnls}, \code{lm}, \code{lme},
   \code{lmList}, \code{nlme}, \code{nlsList}, or \code{nls}.}
 \item{test}{an optional logical value controlling whether likelihood
   ratio tests should be used to compare the fitted models represented
   by \code{object} and the objects in \code{\dots}. Defaults to
   \code{TRUE}.}
 \item{type}{an optional character string specifying the type of sum of
   squares to be used in F-tests for the terms in the model. If 
   \code{"sequential"}, the sequential sum of squares obtained by
   including the terms in the order they appear in the model is used;
   else, if \code{"marginal"}, the marginal sum of squares
   obtained by deleting a term from the model at a time is used. This
   argument is only used when a single fitted object is passed to the
   function. Partial matching of arguments is used, so only the first
   character needs to be provided. Defaults to \code{"sequential"}.}
 \item{adjustSigma}{an optional logical value. If \code{TRUE} and the
   estimation method used to obtain \code{object} was maximum
   likelihood, the residual standard error is multiplied by
   \eqn{\sqrt{n_{obs}/(n_{obs} - n_{par})}}{sqrt(nobs/(nobs - npar))},
   converting it to a REML-like estimate. This argument is only used
   when a single fitted object is passed to the function. Default is
   \code{TRUE}.} 
 \item{Terms}{an optional integer or character vector specifying which
   terms in the model should be jointly tested to be zero using a Wald
   F-test. If given as a character vector, its elements must correspond
   to term names; else, if given as an integer vector, its elements must
   correspond to the order in which terms are included in the
   model. This argument is only used when a single fitted object is
   passed to the function. Default is \code{NULL}.} 
 \item{L}{an optional numeric vector or array specifying linear
   combinations of the coefficients in the model that should be tested
   to be zero. If given as an array, its rows define the linear
   combinations to be tested. If names are assigned to the vector
   elements (array columns), they must correspond to coefficients
   names and will be used to map the linear combination(s) to the
   coefficients; else, if no names are available, the vector elements
   (array columns) are assumed in the same order as the coefficients
   appear in the model. This argument is only used when a single fitted
   object is passed to the function. Default is \code{NULL}.}
 \item{verbose}{an optional logical value. If \code{TRUE}, the calling
   sequences for each fitted model object are printed with the rest of
   the output, being omitted if \code{verbose = FALSE}. Defaults to
   \code{FALSE}.}
}
\description{
  When only one fitted model object is present, a data frame with the
  sums of squares, numerator degrees of freedom, F-values, and P-values
  for Wald tests for the terms in the model (when \code{Terms} and
  \code{L} are \code{NULL}), a combination of model terms (when
  \code{Terms} in not \code{NULL}), or linear combinations of the model
  coefficients (when \code{L} is not \code{NULL}). Otherwise, when
  multiple fitted objects are being compared, a data frame with
  the degrees of freedom, the (restricted) log-likelihood, the 
  Akaike Information Criterion (AIC), and the Bayesian Information
  Criterion (BIC) of each object is returned. If \code{test=TRUE},
  whenever two consecutive  objects have different number of degrees of
  freedom, a likelihood ratio statistic, with the associated p-value is
  included in the returned data frame.   
}
\value{
  a data frame inheriting from class \code{anova.lme}.
}

\references{
  Pinheiro, J. C. and Bates, D. M. (2000), \emph{Mixed-Effects Models in S
    and S-PLUS}, Springer, New York.  
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\note{
  Likelihood comparisons are not meaningful for objects fit using
  restricted maximum likelihood and with different fixed effects.
}

\seealso{\code{\link{gls}}, \code{\link{gnls}},
  \code{\link{lme}}, \code{\link{logLik.gls}},
  \code{\link{AIC}}, \code{\link{BIC}},
  \code{\link{print.anova.lme}}
}

\examples{
# AR(1) errors within each Mare
fm1 <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
           correlation = corAR1(form = ~ 1 | Mare))
anova(fm1)
# variance changes with a power of the absolute fitted values?
fm2 <- update(fm1, weights = varPower())
anova(fm1, fm2)

# Pinheiro and Bates, p. 251-252
fm1Orth.gls <- gls(distance ~ Sex * I(age - 11), Orthodont,
                correlation = corSymm(form = ~ 1 | Subject),
                weights = varIdent(form = ~ 1 | age))
fm2Orth.gls <- update(fm1Orth.gls,
                corr = corCompSymm(form = ~ 1 | Subject))
anova(fm1Orth.gls, fm2Orth.gls)

# Pinheiro and Bates, pp. 215-215, 255-260
#p. 215
fm1Dial.lme <-
  lme(rate ~(pressure + I(pressure^2) + I(pressure^3) + I(pressure^4))*QB,
      Dialyzer, ~ pressure + I(pressure^2))
# p. 216
fm2Dial.lme <- update(fm1Dial.lme,
                  weights = varPower(form = ~ pressure))
# p. 255
fm1Dial.gls <- gls(rate ~ (pressure +
     I(pressure^2) + I(pressure^3) + I(pressure^4))*QB,
        Dialyzer)
fm2Dial.gls <- update(fm1Dial.gls,
                 weights = varPower(form = ~ pressure))
anova(fm1Dial.gls, fm2Dial.gls)
fm3Dial.gls <- update(fm2Dial.gls,
                    corr = corAR1(0.771, form = ~ 1 | Subject))
anova(fm2Dial.gls, fm3Dial.gls)
# anova.gls to compare a gls and an lme fit 
anova(fm3Dial.gls, fm2Dial.lme, test = FALSE)

# Pinheiro and Bates, pp. 261-266
fm1Wheat2 <- gls(yield ~ variety - 1, Wheat2)
fm3Wheat2 <- update(fm1Wheat2,
      corr = corRatio(c(12.5, 0.2),
        form = ~ latitude + longitude, nugget = TRUE))
# Test a specific contrast 
anova(fm3Wheat2, L = c(-1, 0, 1))

}
\keyword{models}
