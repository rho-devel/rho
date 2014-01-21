\name{gam.models}
\alias{gam.models}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Specifying generalized additive models}
\description{ This page is intended to provide some more information on
 how to specify GAMs. A GAM is a GLM in which the linear predictor depends, 
in part, on a sum of smooth functions of predictors and (possibly) linear 
functionals of smooth functions of (possibly dummy) predictors.

Specifically let \eqn{y_i}{y_i} denote an independent random variable 
with mean \eqn{\mu_i}{mu_i} and an exponential family distribution, or failing 
that a known mean variance relationship suitable for use of quasi-likelihood methods. 
Then the the linear predictor of a GAM has a structure something like
 
\deqn{g(\mu_i) = {\bf X}_i{\beta} + f_1(x_{1i},x_{2i}) + f_2(x_{3i}) + L_i f_3(x_4) + 
\ldots}{g(mu_i)=X_i b + f_1(x_1i,x_2i) + f_2(x_3i) + L_i f_3(x_4) + ...}

where \eqn{g}{g} is a known smooth monotonic `link' function, \eqn{{\bf X}_i\beta}{X_i b} 
is the parametric part of the linear predictor, the \eqn{x_j}{x_j} are predictor variables,
the \eqn{f_j}{f_j} are smooth functions and \eqn{L_i}{L_i} is some linear functional of 
\eqn{f_3}{f_3}. There may of course be multiple linear functional terms, or none.

The key idea here is that the
dependence of the response on the predictors can be represented as a
parametric sub-model plus the sum of some (functionals of) smooth functions of one or
more of the predictor variables. Thus the model is quite flexible
relative to strictly parametric linear or generalized linear models,
but still has much more structure than the completely general model
that says that the response is just some smooth function of all the
covariates.

Note one important point. In order for the model to be identifiable
the smooth functions usually have to be constrained to have zero mean (usually
taken over the set of covariate values). The constraint is needed if the term involving the 
smooth includes a constant function in its span. \code{gam} always applies such constraints 
unless there is a \code{by} variable present, in which case an assessment is made of whether 
the constraint is needed or not (see below).

The following sections discuss specifying model structures for \code{gam}. 
Specification of the distribution and link function is done using the \code{\link{family}} 
 argument to \code{\link{gam}} and works in the same way as for \code{\link{glm}}. 
This page therefore concentrates on the model formula for \code{gam}.

}

\section{Models with simple smooth terms}{
Consider the example model.
\deqn{g(\mu_i) = \beta_0 + \beta_1 x_{1i} + \beta_2 x_{2i} + f_1(x_{3i}) + 
f_2(x_{4i},x_{5i})}{ g(mu_i) = b_0 + b_1 x_1i + b_2 x_2i + f1(x_3i) + f2(x_4i,x_5i)}
where the response variables \eqn{y_i}{y_i} has expectation \eqn{\mu_i}{mu_i}
and \eqn{g}{g} is a link function.


The \code{gam} formula for this would be \cr
  \code{y ~ x1 + x2 + s(x3) + s(x4,x5)}. \cr
  This would use the default basis for the smooths (a thin plate
  regression spline basis for each), with automatic selection of the
  effective degrees of freedom for both smooths. The dimension of the
  smoothing basis is given a default value as well (the dimension of the
  basis sets an upper limit on the maximum possible degrees of
  freedom for the basis - the limit is typically one less than basis
  dimension). Full details of how to control smooths are given in
  \code{\link{s}} and \code{\link{te}}, and further discussion of basis
  dimension choice can be found in \code{\link{choose.k}}. 
  For the moment suppose that we would like to change
  the basis of the first smooth to a cubic regression spline basis with
  a dimension of 20, while fixing the second term at 25 degrees of
  freedom. The appropriate formula would be:\cr
  \code{y ~ x1 + x2 + s(x3,bs="cr",k=20) + s(x4,x5,k=26,fx=TRUE)}.

The above assumes that \eqn{x_{4}}{x_4} and \eqn{x_5}{x_5} are naturally on 
similar scales (e.g. they might be co-ordinates), so that isotropic smoothing 
is appropriate. If this assumption is false then tensor product smoothing might be 
better (see \code{\link{te}}). \cr
\code{y ~ x1 + x2 + s(x3) + te(x4,x5)}\cr
would generate a tensor product smooth of \eqn{x_{4}}{x_4} and \eqn{x_5}{x_5}. 
By default this smooth would have basis dimension 25 and use cubic regression spline marginals. 
Varying the defaults is easy. For example\cr
\code{y ~ x1 + x2 + s(x3) + te(x4,x5,bs=c("cr","ps"),k=c(6,7))}\cr
specifies that the tensor product should use a rank 6 cubic regression spline marginal
and a rank 7 P-spline marginal to create a smooth with basis dimension 42.
}

\section{Nested terms/functional ANOVA}{

Sometimes it is interesting to specify smooth models with a main effects + interaction structure such as 
  \deqn{E(y_i) = f_1(x_i) + f_2(z_i) + f_3(x_i,z_i)}{E(y) = f1 (x) + f2(z) + f3(x,z)}
  or
  \deqn{E(y_i)=f_1(x_i) + f_2(z_i) + f_3(v_i)
+ f_4(x_i,z_i) + f_5(z_i,v_i) + f_6(z_i,v_i) + f_7(x_i,z_i,v_i) }{
 E(y) = f1(x) + f2(z) + f3(v) + f4(x,z) + f5(z,v) + f6(z,v) + f7(x,z,v)}
for example. Such models should be set up using \code{\link{ti}} terms in the model formula. For example: \cr
  \code{y ~ ti(x) + ti(z) + ti(x,z)}, or\cr
 \code{y ~ ti(x) + ti(z) + ti(v) + ti(x,z) + ti(x,v) + ti(z,v)+ti(x,z,v)}. \cr
The \code{ti} terms produce interactions with the component main effects excluded appropriately. (There is in fact no need to use \code{ti} terms for the main effects here, \code{s} terms could also be used.)

\code{gam} allows nesting (or `overlap') of \code{te} and \code{s} smooths, and automatically generates side conditions to 
make such models identifiable, but the resulting models are much less stable and interpretable than those constructed using \code{ti} terms. 

}

\section{`by' variables}{
 \code{by} variables are the means for constructing `varying-coefficient models' (geographic regression models) and 
for letting smooths `interact' with factors or parametric terms. They are also the key to specifying general linear 
functionals of smooths.

The \code{\link{s}} and \code{\link{te}} terms used to specify smooths accept an argument \code{by}, 
which is a numeric or factor variable of the same dimension as the covariates of the smooth. 
If a \code{by} variable is numeric, then its \eqn{i^{th}}{ith} element multiples the  \eqn{i^{th}}{ith}
row of the model matrix corresponding to the smooth term concerned. 

Factor smooth interactions (see also \code{\link{factor.smooth.interaction}}).
 If a \code{by} variable is a \code{\link{factor}} then it 
generates an indicator vector for each level 
of the factor, unless it is an \code{\link{ordered}} factor.
In the non-ordered case, the model matrix for the smooth term is then replicated for each factor level,
and each copy has its rows multiplied by the corresponding rows of its
indicator variable. The smoothness penalties are also duplicated for each
factor level.  In short a different smooth is generated
for each factor level (the \code{id} argument to \code{\link{s}} and \code{\link{te}} can be used to force all 
such smooths to have the same smoothing parameter). \code{\link{ordered}} \code{by} variables are handled in the same 
way, except that no smooth is generated for the first level of the ordered factor (see \code{b3} example below). 
This is useful for setting up 
identifiable models when the same smooth occurs more than once in a model, with different factor \code{by} variables.

 
As an example, consider the model
\deqn{E(y_i) = \beta_0+ f(x_i)z_i}{E(y_i) = b_0 + f(x_i)z_i}
where \eqn{f}{f} is a smooth function, and \eqn{z_i}{z_i} is a numeric variable.
The appropriate formula is:\cr
\code{y ~ s(x,by=z)}\cr
- the \code{by} argument ensures that the smooth function gets multiplied by
covariate \code{z}. Note that when using factor by variables, centering constraints are applied to the smooths,
which usually means that the by variable should be included as a parametric term, as well.

The example code below also illustrates the use of factor \code{by} variables.

\code{by} variables may be supplied as numeric matrices as part of specifying general linear functional terms.

If a \code{by} variable is present and numeric (rather than a factor) then the corresponding smooth is only subjected 
to an identifiability constraint if (i) the \code{by} variable is a constant vector, or, (ii) for a matrix 
\code{by} variable, \code{L}, if \code{L\%*\%rep(1,ncol(L))} is constant or (iii) if a user defined smooth constructor 
supplies an identifiability constraint explicitly, and that constraint has an attibute \code{"always.apply"}. 
}

\section{Linking smooths with `id'}{
It is sometimes desirable to insist that different smooth terms have the same degree of smoothness. 
This can be done by using the \code{id} argument to \code{\link{s}} or \code{\link{te}} terms. Smooths 
which share an \code{id} will have the same smoothing parameter. Really this only makes sense if the 
smooths use the same basis functions, and the default behaviour is to force this to happen: all smooths 
sharing an \code{id} have the same basis functions as the first smooth occurring with that \code{id}. Note 
that if you want exactly the same function for each smooth, then this is best achieved by making use of the 
summation convention covered under `linear functional terms'. 

As an example suppose that \eqn{E(y_i)\equiv\mu_i}{E(y_i)=mu_i} and 
\deqn{g(\mu_i) = f_1(x_{1i}) + f_2(x_{2i},x_{3i}) + 
f_3(x_{4i})}{g(mu_i) = f1(x_1i) + f2(x_2i,x_3i) + f3(x_4i)}
but that \eqn{f_1}{f1} and \eqn{f_3}{f3} should have the same smoothing parameters (and \eqn{x_2}{x_2}
and \eqn{x_3}{x_3} are on different scales). Then 
the \code{gam} formula\cr
\code{y ~ s(x1,id=1) + te(x_2,x3) + s(x4,id=1)}\cr
would achieve the desired result. \code{id} can be numbers or character strings. Giving an \code{id} to a 
term with a factor \code{by} variable causes the smooths at each level of the factor to have the same smoothing 
parameter.

Smooth term \code{id}s are not supported by \code{gamm}.
}

\section{Linear functional terms}{

General linear functional terms have a long history in the spline literature including in the penalized 
GLM context (see e.g. Wahba 1990). Such terms encompass varying coefficient models/ geographic 
regression, functional GLMs (i.e. GLMs with functional predictors), GLASS models, etc, and allow 
smoothing with respect to aggregated covariate values, for example. 

Such terms are implemented in \code{mgcv} using a simple `summation convention' for smooth terms: If the covariates of a 
smooth are supplied as matrices, then summation of the evaluated smooth over the columns of the matrices is implied. Each 
covariate matrix and any \code{by} variable matrix must be of the same dimension. Consider, for example the term\cr
\code{s(X,Z,by=L)}\cr
where \code{X}, \code{Z} and \code{L} are \eqn{n \times p}{n by p} matrices. Let \eqn{f}{f} denote the thin plate regression 
spline specified. The resulting contibution to the \eqn{i^{\rm th}}{ith} 
element of the linear predictor is 
\deqn{\sum_{j=1}^p L_{ij}f(X_{ij},Z_{ij})}{sum_j^p L_ij f(X_ij,Z_ij)}
If no \code{L} is supplied then all its elements are taken as 1. In R code terms, let \code{F} denote the \eqn{n \times p}{n by p} 
matrix obtained by evaluating the smooth at the values in \code{X} and \code{Z}. Then the contribution of the term to the 
linear predictor is \code{rowSums(L*F)} (note that it's element by element multiplication here!). 


The summation convention applies to \code{te} terms as well as \code{s} terms. More details and examples 
are provided in 
\code{\link{linear.functional.terms}}. 
}

\section{Random effects}{
Random effects can be added to \code{gam} models using \code{s(...,bs="re")} terms (see 
\code{\link{smooth.construct.re.smooth.spec}}), 
or the \code{paraPen} argument to \code{\link{gam}} covered below. See \code{\link{gam.vcomp}}, \code{\link{random.effects}} and 
\code{\link{smooth.construct.re.smooth.spec}} for further details. An alternative is to use the approach of \code{\link{gamm}}.

}

\section{Penalizing the parametric terms}{
In case the ability to add smooth classes, smooth identities, \code{by} variables and the summation convention are 
still not sufficient to implement exactly the penalized GLM that you require, \code{\link{gam}} also allows you to penalize the 
parametric terms in the model formula. This is mostly useful in 
allowing one or more matrix terms to be included in the formula, along with a 
sequence of quadratic penalty matrices for each. 

Suppose that you have set up a model matrix \eqn{\bf X}{X}, and want to penalize the corresponding coefficients, \eqn{\beta}{b}
with two penalties \eqn{\beta^T {\bf S}_1 \beta}{b'S1 b} and \eqn{\beta^T {\bf S}_2 \beta}{b'S2 b}. 
Then something like the 
following would be appropriate:\cr
\code{gam(y ~ X - 1,paraPen=list(X=list(S1,S2)))}\cr
The \code{paraPen} argument should be a list with elements having  names corresponding to the terms being penalized. 
Each element of \code{paraPen} is itself a list, with optional elements \code{L}, \code{rank} and \code{sp}: all other elements 
must be penalty matrices. If present, \code{rank} is a vector giving the rank of each penalty matrix 
(if absent this is determined numerically). \code{L} is a matrix that maps underlying log smoothing parameters to the 
log smoothing parameters that actually multiply the individual quadratic penalties: taken as the identity if not supplied.
\code{sp} is a vector of (underlying) smoothing parameter values: positive values are taken as fixed, negative to signal that 
the smoothing parameter should be estimated. Taken as all negative if not supplied.

An obvious application of \code{paraPen} is to incorporate random effects, and an example of this is provided below. In this
case the supplied penalty matrices will be (generalized) inverse covariance matrices for the random effects --- i.e. precision 
matrices. The final estimate of the covariance matrix corresponding to one of these penalties is given by the (generalized) 
inverse of the penalty matrix multiplied by the estimated scale parameter and divided by the estimated 
smoothing parameter for the penalty. For example, if you use an identity matrix to penalize some coefficients that are to be viewed as i.i.d. 
Gaussian random effects, then their estimated variance will be the estimated scale parameter divided by the estimate of the 
smoothing parameter, for this penalty. See the `rail' example below. 

P-values for penalized parametric terms should be treated with caution. If you must have them, then use the option \code{freq=TRUE} in 
\code{\link{anova.gam}} and \code{\link{summary.gam}}, which will tend to give reasonable results for random effects implemented this way, 
but not for terms with a rank defficient penalty (or penalties with a wide eigen-spectrum). 
}



%- maybe also `usage' for other objects documented here.

\references{
Wahba (1990) Spline Models of Observational Data SIAM.

Wood S.N. (2006) Generalized Additive Models: An Introduction with R. Chapman
and Hall/CRC Press.
}

\author{ Simon N. Wood \email{simon.wood@r-project.org}}

\examples{
require(mgcv)
set.seed(10)
## simulate date from y = f(x2)*x1 + error
dat <- gamSim(3,n=400)

b<-gam(y ~ s(x2,by=x1),data=dat)
plot(b,pages=1)
summary(b)

## Factor `by' variable example (with a spurious covariate x0)
## simulate data...

dat <- gamSim(4)

## fit model...
b <- gam(y ~ fac+s(x2,by=fac)+s(x0),data=dat)
plot(b,pages=1)
summary(b)

## note that the preceding fit is the same as....
b1<-gam(y ~ s(x2,by=as.numeric(fac==1))+s(x2,by=as.numeric(fac==2))+
            s(x2,by=as.numeric(fac==3))+s(x0)-1,data=dat)
## ... the `-1' is because the intercept is confounded with the 
## *uncentred* smooths here.
plot(b1,pages=1)
summary(b1)

## repeat forcing all s(x2) terms to have the same smoothing param
## (not a very good idea for these data!)
b2 <- gam(y ~ fac+s(x2,by=fac,id=1)+s(x0),data=dat)
plot(b2,pages=1)
summary(b2)

## now repeat with a single reference level smooth, and 
## two `difference' smooths...
dat$fac <- ordered(dat$fac)
b3 <- gam(y ~ fac+s(x2)+s(x2,by=fac)+s(x0),data=dat,method="REML")
plot(b3,pages=1)
summary(b3)


rm(dat)

## An example of a simple random effects term implemented via 
## penalization of the parametric part of the model...

dat <- gamSim(1,n=400,scale=2) ## simulate 4 term additive truth
## Now add some random effects to the simulation. Response is 
## grouped into one of 20 groups by `fac' and each groups has a
## random effect added....
fac <- as.factor(sample(1:20,400,replace=TRUE))
dat$X <- model.matrix(~fac-1)
b <- rnorm(20)*.5
dat$y <- dat$y + dat$X\%*\%b

## now fit appropriate random effect model...
PP <- list(X=list(rank=20,diag(20)))
rm <- gam(y~ X+s(x0)+s(x1)+s(x2)+s(x3),data=dat,paraPen=PP)
plot(rm,pages=1)
## Get estimated random effects standard deviation...
sig.b <- sqrt(rm$sig2/rm$sp[1]);sig.b 

## a much simpler approach uses "re" terms...

rm1 <- gam(y ~ s(fac,bs="re")+s(x0)+s(x1)+s(x2)+s(x3),data=dat,method="ML")
gam.vcomp(rm1)

## Simple comparison with lme, using Rail data. 
## See ?random.effects for a simpler method
require(nlme)
b0 <- lme(travel~1,data=Rail,~1|Rail,method="ML") 
Z <- model.matrix(~Rail-1,data=Rail,
     contrasts.arg=list(Rail="contr.treatment"))
b <- gam(travel~Z,data=Rail,paraPen=list(Z=list(diag(6))),method="ML")

b0 
(b$reml.scale/b$sp)^.5 ## `gam' ML estimate of Rail sd
b$reml.scale^.5         ## `gam' ML estimate of residual sd

b0 <- lme(travel~1,data=Rail,~1|Rail,method="REML") 
Z <- model.matrix(~Rail-1,data=Rail,
     contrasts.arg=list(Rail="contr.treatment"))
b <- gam(travel~Z,data=Rail,paraPen=list(Z=list(diag(6))),method="REML")

b0 
(b$reml.scale/b$sp)^.5 ## `gam' REML estimate of Rail sd
b$reml.scale^.5         ## `gam' REML estimate of residual sd

}
\keyword{models} \keyword{regression}%-- one or more ..


