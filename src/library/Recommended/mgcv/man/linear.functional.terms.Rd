\name{linear.functional.terms}
\alias{linear.functional.terms}
\alias{function.predictors}
\alias{signal.regression}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Linear functionals of a smooth in GAMs}

\description{\code{\link{gam}} allows the response variable to depend on linear 
functionals of smooth terms. Specifically dependancies of the form
\deqn{g(\mu_i) = \ldots + \sum_j L_{ij} f(x_{ij}) + \ldots }{g(mu_i) = ... + sum_j L_ij f(x_ij) +...}
are allowed, where the \eqn{x_{ij}}{x_ij} are covariate values and the \eqn{L_{ij}}{L_ij} are 
fixed weights. i.e. the response can depend on the weighted sum of the same smooth 
evaluated at different covariate values. This allows, for example, for the 
response to depend on the derivatives or integrals of a smooth (approximated by finite 
differencing or quadrature, respectively). It also allows dependence on predictor functions 
(sometimes called `signal regression').

The mechanism by which this is achieved is to supply matrices of covariate values to the 
model smooth terms specified by \code{\link{s}} or \code{\link{te}} terms in the model formula.
Each column of the covariate matrix gives rise to a corresponding column of predictions 
from the smooth. Let the resulting matrix of evaluated smooth values be F (F will have 
the same dimension as the covariate matrices). In the absense of a \code{by} variable 
then these columns are simply summed and added to the linear predictor. i.e. the contribution of the 
term to the linear predictor is \code{rowSums(F)}. If a \code{by} variable is present 
then it must be a matrix, L,say, of the same dimension as F (and the covariate matrices), 
and it contains the weights \eqn{L_{ij}}{L_ij} in the summation given above. 
So in this case the contribution to the  linear predictor is \code{rowSums(L*F)}. 

Note that if a \eqn{{\bf L1}}{L1} (i.e. \code{rowSums(L)}) is a constant vector, or there is no \code{by} 
variable then the smooth will automatically be centred in order to ensure identifiability. Otherwise it 
will not be. Note also that for centred smooths it can be worth replacing the constant term in the model with \code{rowSums(L)} 
in order to ensure that predictions are automatically on the right scale.

When predicting from the model it is not necessary to provide matrix covariate and \code{by} variable values. 
For example to simply examine the underlying smooth function one would use vectors of covariate values and vector 
\code{by} variables, with the \code{by} variable and equivalent of \code{L1}, above, set to vectors of ones. 

}

%- maybe also `usage' for other objects documented here.

\author{ Simon N. Wood \email{simon.wood@r-project.org}}

\examples{
### matrix argument `linear operator' smoothing
library(mgcv)
set.seed(0)

###############################
## simple summation example...#
###############################

n<-400
sig<-2
x <- runif(n, 0, .9)
f2 <- function(x) 0.2*x^11*(10*(1-x))^6+10*(10*x)^3*(1-x)^10
x1 <- x + .1

f <- f2(x) + f2(x1)  ## response is sum of f at two adjacent x values 
y <- f + rnorm(n)*sig

X <- matrix(c(x,x1),n,2) ## matrix covariate contains both x values
b <- gam(y~s(X))         

plot(b)  ## reconstruction of f
plot(f,fitted(b))

######################################################################
## multivariate integral example. Function `test1' will be integrated# 
## (by midpoint quadrature) over 100 equal area sub-squares covering # 
## the unit square. Noise is added to the resulting simulated data.  #
## `test1' is estimated from the resulting data using two alternative#
## smooths.                                                          #
######################################################################

test1 <- function(x,z,sx=0.3,sz=0.4)
  { (pi**sx*sz)*(1.2*exp(-(x-0.2)^2/sx^2-(z-0.3)^2/sz^2)+
    0.8*exp(-(x-0.7)^2/sx^2-(z-0.8)^2/sz^2))
  }

## create quadrature (integration) grid, in useful order
ig <- 5 ## integration grid within square
mx <- mz <- (1:ig-.5)/ig
ix <- rep(mx,ig);iz <- rep(mz,rep(ig,ig))

og <- 10 ## observarion grid
mx <- mz <- (1:og-1)/og
ox <- rep(mx,og);ox <- rep(ox,rep(ig^2,og^2))
oz <- rep(mz,rep(og,og));oz <- rep(oz,rep(ig^2,og^2))

x <- ox + ix/og;z <- oz + iz/og ## full grid, subsquare by subsquare

## create matrix covariates...
X <- matrix(x,og^2,ig^2,byrow=TRUE)
Z <- matrix(z,og^2,ig^2,byrow=TRUE)

## create simulated test data...
dA <- 1/(og*ig)^2  ## quadrature square area
F <- test1(X,Z)    ## evaluate on grid
f <- rowSums(F)*dA ## integrate by midpoint quadrature
y <- f + rnorm(og^2)*5e-4 ## add noise
## ... so each y is a noisy observation of the integral of `test1'
## over a 0.1 by 0.1 sub-square from the unit square

## Now fit model to simulated data...

L <- X*0 + dA

## ... let F be the matrix of the smooth evaluated at the x,z values
## in matrices X and Z. rowSums(L*F) gives the model predicted
## integrals of `test1' corresponding to the observed `y'

L1 <- rowSums(L) ## smooths are centred --- need to add in L%*%1

## fit models to reconstruct `test1'....

b <- gam(y~s(X,Z,by=L)+L1-1)   ## (L1 and const are confounded here)
b1 <- gam(y~te(X,Z,by=L)+L1-1) ## tensor product alternative

## plot results...

old.par<-par(mfrow=c(2,2))
x<-runif(n);z<-runif(n);
xs<-seq(0,1,length=30);zs<-seq(0,1,length=30)
pr<-data.frame(x=rep(xs,30),z=rep(zs,rep(30,30)))
truth<-matrix(test1(pr$x,pr$z),30,30)
contour(xs,zs,truth)
plot(b)
vis.gam(b,view=c("X","Z"),cond=list(L1=1,L=1),plot.type="contour")
vis.gam(b1,view=c("X","Z"),cond=list(L1=1,L=1),plot.type="contour")

####################################
## A "signal" regression example...#
####################################

rf <- function(x=seq(0,1,length=100)) {
## generates random functions...
  m <- ceiling(runif(1)*5) ## number of components
  f <- x*0;
  mu <- runif(m,min(x),max(x));sig <- (runif(m)+.5)*(max(x)-min(x))/10
  for (i in 1:m) f <- f+ dnorm(x,mu[i],sig[i])
  f
}

x <- seq(0,1,length=100) ## evaluation points

## example functional predictors...
par(mfrow=c(3,3));for (i in 1:9) plot(x,rf(x),type="l",xlab="x")

## simulate 200 functions and store in rows of L...
L <- matrix(NA,200,100) 
for (i in 1:200) L[i,] <- rf()  ## simulate the functional predictors

f2 <- function(x) { ## the coefficient function
  (0.2*x^11*(10*(1-x))^6+10*(10*x)^3*(1-x)^10)/10 
}

f <- f2(x) ## the true coefficient function

y <- L\%*\%f + rnorm(200)*20 ## simulated response data

## Now fit the model E(y) = L\%*\%f(x) where f is a smooth function.
## The summation convention is used to evaluate smooth at each value
## in matrix X to get matrix F, say. Then rowSum(L*F) gives E(y).

## create matrix of eval points for each function. Note that
## `smoothCon' is smart and will recognize the duplication...
X <- matrix(x,200,100,byrow=TRUE) 

b <- gam(y~s(X,by=L,k=20)) 
par(mfrow=c(1,1))
plot(b,shade=TRUE);lines(x,f,col=2)

}
\keyword{models} \keyword{regression}%-- one or more ..


