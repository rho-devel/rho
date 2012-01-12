% File nlme/man/anova.lme.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: anova.lme.Rd,v 1.6.2.1 2003/04/17 22:28:45 bates Exp $
\name{anova.lme}
\alias{anova.lme}
\alias{print.anova.lme}
\title{Compare Likelihoods of Fitted Objects}
\usage{
\method{anova}{lme}(object, \dots, test, type, adjustSigma, Terms, L, verbose)
\method{print}{anova.lme}(x, verbose, \dots)
}
\arguments{
 \item{object}{a fitted model object inheriting from class \code{lme},
   representing a mixed-effects model.}
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
 \item{x}{an object inheriting from class \code{anova.lme}}
 \item{verbose}{an optional logical value. If \code{TRUE}, the calling
   sequences for each fitted model object are printed with the rest of
   the output, being omitted if \code{verbose = FALSE}. Defaults to
   \code{FALSE}.}
}
\description{
  When only one fitted model object is present, a data frame with the
  sums of squares, numerator degrees of freedom, denominator degrees of
  freedom, F-values, and P-values for Wald tests for the terms in the
  model (when \code{Terms} and \code{L} are \code{NULL}), a combination
  of model terms (when \code{Terms} in not \code{NULL}), or linear
  combinations of the model coefficients (when \code{L} is not
  \code{NULL}). Otherwise, when multiple fitted objects are being
  compared, a data frame with the degrees of freedom, the (restricted)
  log-likelihood, the Akaike Information Criterion (AIC), and the
  Bayesian Information Criterion (BIC) of each object is returned. If
  \code{test=TRUE}, whenever two consecutive  objects have different
  number of degrees of freedom, a likelihood ratio statistic, with the
  associated p-value is included in the returned data frame.   
}
\value{
  a data frame inheriting from class \code{anova.lme}.
}

\references{
  Pinheiro, J.C., and Bates, D.M. (2000) "Mixed-Effects Models
  in S and S-PLUS", Springer.  
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\note{
  Likelihood comparisons are not meaningful for objects fit using
  restricted maximum likelihood and with different fixed effects.
}

\seealso{\code{\link{gls}}, \code{\link{gnls}}, \code{\link{nlme}},
  \code{\link{lme}}, \code{\link{AIC}}, \code{\link{BIC}},
  \code{\link{print.anova.lme}},
  \code{\link{logLik.lme}},
}  

\examples{
fm1 <- lme(distance ~ age, Orthodont, random = ~ age | Subject)
anova(fm1)
fm2 <- update(fm1, random = pdDiag(~age))
anova(fm1, fm2)

# Pinheiro and Bates, pp. 251-254
fm1Orth.gls <- gls(distance ~ Sex * I(age - 11), Orthodont,
            correlation = corSymm(form = ~ 1 | Subject),
            weights = varIdent(form = ~ 1 | age))
fm2Orth.gls <- update(fm1Orth.gls,
                corr = corCompSymm(form = ~ 1 | Subject))
# anova.gls
anova(fm1Orth.gls, fm2Orth.gls)
fm3Orth.gls <- update(fm2Orth.gls, weights = NULL)
# anova.gls
anova(fm2Orth.gls, fm3Orth.gls)
fm4Orth.gls <- update(fm3Orth.gls,
                 weights = varIdent(form = ~ 1 | Sex))
# anova.gls
anova(fm3Orth.gls, fm4Orth.gls)
# not in book but needed for the following command
fm3Orth.lme <-
    lme(distance~Sex*I(age-11), data = Orthodont,
        random = ~ I(age-11) | Subject,
        weights = varIdent(form = ~ 1 | Sex))
# anova.lme to compare an "lme" object with a "gls" object 
anova(fm3Orth.lme, fm4Orth.gls, test = FALSE)

# Pinheiro and Bates, pp. 222-225
options(contrasts = c("contr.treatment", "contr.poly"))
fm1BW.lme <- lme(weight ~ Time * Diet, BodyWeight,
                   random = ~ Time)
fm2BW.lme <- update(fm1BW.lme, weights = varPower())
# Test a specific contrast
anova(fm2BW.lme, L = c("Time:Diet2" = 1, "Time:Diet3" = -1))

fm1Theo.lis <- nlsList(
     conc ~ SSfol(Dose, Time, lKe, lKa, lCl), data=Theoph)
fm1Theo.lis

# Pinheiro and Bates, pp. 352-365
fm1Theo.lis <- nlsList(
     conc ~ SSfol(Dose, Time, lKe, lKa, lCl), data=Theoph)
fm1Theo.nlme <- nlme(fm1Theo.lis)
fm2Theo.nlme <- update(fm1Theo.nlme,
   random=pdDiag(lKe+lKa+lCl~1) )
fm3Theo.nlme <- update(fm2Theo.nlme,
   random=pdDiag(lKa+lCl~1) )

# anova comparing 3 models
anova(fm1Theo.nlme, fm3Theo.nlme, fm2Theo.nlme)

}
\keyword{models}
