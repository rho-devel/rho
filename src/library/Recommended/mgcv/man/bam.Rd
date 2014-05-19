\name{bam}
\alias{bam}

%- Also NEED an `\alias' for EACH other topic documented here.
\title{Generalized additive models for very large datasets}

\description{ Fits a generalized additive model (GAM) to a very large
data set, the term `GAM' being taken to include any quadratically penalized GLM.  
The degree of smoothness of model terms is estimated as part of
fitting. In use the function is much like \code{\link{gam}}, except that the numerical methods
are designed for datasets containing upwards of several tens of thousands of data. The advantage 
of \code{bam} is much lower memory footprint than \code{\link{gam}}, but it can also be much faster, 
for large datasets. \code{bam} can also compute on a cluster set up by the \link[parallel]{parallel} package.

}
\usage{
bam(formula,family=gaussian(),data=list(),weights=NULL,subset=NULL,
    na.action=na.omit, offset=NULL,method="fREML",control=list(),
    scale=0,gamma=1,knots=NULL,sp=NULL,min.sp=NULL,paraPen=NULL,
    chunk.size=10000,rho=0,AR.start=NULL,sparse=FALSE,cluster=NULL,gc.level=1,
    use.chol=FALSE,samfrac=1,...)
}
%- maybe also `usage' for other objects documented here.

\arguments{ 
\item{formula}{ A GAM formula (see \code{\link{formula.gam}} and also \code{\link{gam.models}}). 
This is exactly like the formula for a GLM except that smooth terms, \code{s} and \code{te} can be added 
to the right hand side to specify that the linear predictor depends on smooth functions of predictors 
(or linear functionals of these).
} 

\item{family}{
This is a family object specifying the distribution and link to use in
fitting etc. See \code{\link{glm}} and \code{\link{family}} for more
details. A negative binomial family is provided: see \code{\link{negbin}}, but
only the known theta case is supported by \code{bam}.
} 

\item{data}{ A data frame or list containing the model response variable and 
covariates required by the formula. By default the variables are taken 
from \code{environment(formula)}: typically the environment from 
which \code{gam} is called.} 

\item{weights}{ prior weights on the data.}

\item{subset}{ an optional vector specifying a subset of observations to be
          used in the fitting process.}

\item{na.action}{ a function which indicates what should happen when the data
          contain `NA's.  The default is set by the `na.action' setting
          of `options', and is `na.fail' if that is unset.  The
          ``factory-fresh'' default is `na.omit'.}

\item{offset}{Can be used to supply a model offset for use in fitting. Note
that this offset will always be completely ignored when predicting, unlike an offset 
included in \code{formula}: this conforms to the behaviour of
\code{lm} and \code{glm}.}



\item{method}{The smoothing parameter estimation method. \code{"GCV.Cp"} to use GCV for unknown scale parameter and
Mallows' Cp/UBRE/AIC for known scale. \code{"GACV.Cp"} is equivalent, but using GACV in place of GCV. \code{"REML"} 
for REML estimation, including of unknown scale, \code{"P-REML"} for REML estimation, but using a Pearson estimate 
of the scale. \code{"ML"} and \code{"P-ML"} are similar, but using maximum likelihood in place of REML. Default 
\code{"fREML"} uses fast REML computation.}

\item{control}{A list of fit control parameters to replace defaults returned by 
\code{\link{gam.control}}. Any control parameters not supplied stay at their default values.}


\item{scale}{ If this is positive then it is taken as the known scale parameter. Negative signals that the 
scale paraemter is unknown. 0 signals that the scale parameter is 1  for Poisson and binomial and unknown otherwise. 
Note that (RE)ML methods can only work with scale parameter 1 for the Poisson and binomial cases.    
} 

\item{gamma}{It is sometimes useful to inflate the model degrees of 
freedom in the GCV or UBRE/AIC score by a constant multiplier. This allows 
such a multiplier to be supplied. } 


\item{knots}{this is an optional list containing user specified knot values to be used for basis construction. 
For most bases the user simply supplies the knots to be used, which must match up with the \code{k} value
supplied (note that the number of knots is not always just \code{k}). 
See \code{\link{tprs}} for what happens in the \code{"tp"/"ts"} case. 
Different terms can use different numbers of knots, unless they share a covariate.
}

\item{sp}{A vector of smoothing parameters can be provided here.
 Smoothing parameters must be supplied in the order that the smooth terms appear in the model 
formula. Negative elements indicate that the parameter should be estimated, and hence a mixture 
of fixed and estimated parameters is possible. If smooths share smoothing parameters then \code{length(sp)} 
must correspond to the number of underlying smoothing parameters.}

\item{min.sp}{Lower bounds can be supplied for the smoothing parameters. Note
that if this option is used then the smoothing parameters \code{full.sp}, in the 
returned object, will need to be added to what is supplied here to get the 
 smoothing parameters actually multiplying the penalties. \code{length(min.sp)} should 
always be the same as the total number of penalties (so it may be longer than \code{sp},
if smooths share smoothing parameters).}


\item{paraPen}{optional list specifying any penalties to be applied to parametric model terms. 
\code{\link{gam.models}} explains more.}


\item{chunk.size}{The model matrix is created in chunks of this size, rather than ever being formed whole.}

\item{rho}{An AR1 error model can be used for the residuals (based on dataframe order), of Gaussian-identity 
           link models. This is the AR1 correlation parameter.}

\item{AR.start}{logical variable of same length as data, \code{TRUE} at first observation of an independent
section of AR1 correlation. Very first observation in data frame does not need this. If \code{NULL} then 
there are no breaks in AR1 correlaion.}

\item{sparse}{If all smooths are P-splines and all tensor products are of the form \code{te(...,bs="ps",np=FALSE)} 
then in principle computation could be made faster using sparse matrix methods, and you could set this to \code{TRUE}. 
In practice the speed up is disappointing, and the computation is less well conditioned than the default. See details.}

\item{cluster}{\code{bam} can compute the computationally dominant QR decomposition in parallel using \link[parallel]{parLapply}
from the \code{parallel} package, if it is supplied with a cluster on which to do this (a cluster here can be some cores of a 
single machine). See details and example code. 
}
\item{gc.level}{to keep the memory footprint down, it helps to call the garbage collector often, but this takes 
a substatial amount of time. Setting this to zero means that garbage collection only happens when R decides it should. Setting to 2 gives frequent garbage collection. 1 is in between.}

\item{use.chol}{By default \code{bam} uses a very stable QR update approach to obtaining the QR decomposition
of the model matrix. For well conditioned models an alternative accumulates the crossproduct of the model matrix
and then finds its Choleski decomposition, at the end. This is somewhat more efficient, computationally.}

\item{samfrac}{For very large sample size Generalized additive models the number of iterations needed for the model fit can 
be reduced by first fitting a model to a random sample of the data, and using the results to supply starting values. This initial fit is run with sloppy convergence tolerances, so is typically very low cost. \code{samfrac} is the sampling fraction to use. 0.1 is often reasonable. }

\item{...}{further arguments for 
passing on e.g. to \code{gam.fit} (such as \code{mustart}). }

}

\value{ 
An object of class \code{"gam"} as described in \code{\link{gamObject}}.
}


\details{ \code{bam} operates by first setting up the basis characteristics for the smooths, using a representative subsample 
of the data. Then the model matrix is constructed in blocks using \code{\link{predict.gam}}. For each block the factor R,
from the QR decomposition of the whole model matrix is updated, along with Q'y. and the sum of squares of y. At the end of 
block processing, fitting takes place, without the need to ever form the whole model matrix. 

In the generalized case, the same trick is used with the weighted model matrix and weighted pseudodata, at each step of the PIRLS.
Smoothness selection is performed on the working model at each stage (performance oriented iteration), to maintain the 
small memory footprint. This is trivial to justify in the case of GCV or Cp/UBRE/AIC based model selection, and 
for REML/ML is justified via the asymptotic multivariate normality of Q'z where z is the IRLS pseudodata. 

Note that POI is not as stable as the default nested iteration used with \code{\link{gam}}, but that for very large, information rich,
datasets, this is unlikely to matter much. 

Note also that it is possible to spend most of the computational time on basis evaluation, if an expensive basis is used. In practice this means that the default \code{"tp"} basis should be avoided: almost any other basis (e.g. \code{"cr"} or \code{"ps"}) 
can be used in the 1D case, and tensor product smooths (\code{te}) are typically much less costly in the multi-dimensional case. 

If \code{cluster} is provided as a cluster set up using \code{\link[parallel]{makeCluster}} (or \code{\link[parallel]{makeForkCluster}}) from the \code{parallel} package, then the rate limiting QR decomposition of the model matrix is performed in parallel using this cluster. Note that the speed ups are often not that great. On a multi-core machine it is usually best to set the cluster size to the number of physical cores, which is often less than what is reported by \code{\link[parallel]{detectCores}}. Using more than the number of physical cores can result in no speed up at all (or even a slow down). Note that a highly parallel BLAS may negate all advantage from using a cluster of cores. Computing in parallel of course requires more memory than computing in series. See examples.

If the argument \code{sparse=TRUE} then QR updating is replaced by an alternative scheme, in which the model matrix is stored whole
 as a sparse matrix. This only makes sense if all smooths are P-splines and all tensor products are of the 
form \code{te(...,bs="ps",np=FALSE)}, but no check is made. The computations are then based on the Choleski decomposition of 
the crossproduct of the sparse model matrix. Although this crossproduct is nearly dense, sparsity should make its 
formation efficient, which is useful as it is the leading order term in the operations count. However there is no benefit 
in using sparse methods to form the Choleski decomposition, given that the crossproduct is dense. 
In practice the sparse matrix handling overheads mean that modest or no speed ups are produced 
by this approach, while the computation is less stable than the default, and the memory footprint often higher 
(but please let the author know if you find an example where the speedup is really worthwhile).

}


\references{
\url{http://www.maths.bath.ac.uk/~sw283/}
}

\author{ Simon N. Wood \email{simon.wood@r-project.org}


}

\section{WARNINGS }{

The routine will be slow if the default \code{"tp"} basis is used. 

You must have more unique combinations of covariates than the model has total
parameters. (Total parameters is sum of basis dimensions plus sum of non-spline 
terms less the number of spline terms). 

This routine is less stable than `gam' for the same dataset.

The negbin family is only supported for the *known theta* case.

} 

\seealso{\code{\link{mgcv-package}}, \code{\link{gamObject}}, \code{\link{gam.models}}, \code{\link{smooth.terms}},
\code{\link{linear.functional.terms}}, \code{\link{s}},
\code{\link{te}} \code{\link{predict.gam}},
\code{\link{plot.gam}}, \code{\link{summary.gam}}, \code{\link{gam.side}},
\code{\link{gam.selection}}, \code{\link{gam.control}}
\code{\link{gam.check}}, \code{\link{linear.functional.terms}} \code{\link{negbin}}, \code{\link{magic}},\code{\link{vis.gam}}
 }

\examples{
library(mgcv)
## Some examples are marked 'Not run' purely to keep 
## checking load on CRAN down. Sample sizes are small for 
## the same reason.

dat <- gamSim(1,n=20000,dist="normal",scale=20)
bs <- "cr";k <- 20
b <- bam(y ~ s(x0,bs=bs,k=k)+s(x1,bs=bs,k=k)+s(x2,bs=bs,k=k)+
           s(x3,bs=bs,k=k),data=dat)
summary(b)
plot(b,pages=1,rug=FALSE)  ## plot smooths, but not rug
plot(b,pages=1,rug=FALSE,seWithMean=TRUE) ## `with intercept' CIs

\dontrun{ 
ba <- bam(y ~ s(x0,bs=bs,k=k)+s(x1,bs=bs,k=k)+s(x2,bs=bs,k=k)+
            s(x3,bs=bs,k=k),data=dat,method="GCV.Cp") ## use GCV
summary(ba)}

## A Poisson example...

k <- 15
dat <- gamSim(1,n=11000,dist="poisson",scale=.1)
system.time(b1 <- bam(y ~ s(x0,bs=bs,k=k)+s(x1,bs=bs,k=k)+s(x2,bs=bs,k=k)+
            s(x3,bs=bs,k=k),data=dat,family=poisson()))
b1

## repeat on a cluster (need much larger n to be worthwhile!)
require(parallel)  
nc <- 2   ## cluster size, set for example portability
if (detectCores()>1) { ## no point otherwise
  cl <- makeCluster(nc) 
  ## could also use makeForkCluster, but read warnings first!
} else cl <- NULL
  
system.time(b2 <- bam(y ~ s(x0,bs=bs,k=k)+s(x1,bs=bs,k=k)+s(x2,bs=bs,k=k)+
       s(x3,bs=bs,k=k),data=dat,family=poisson(),cluster=cl))

## ... first call has startup overheads, repeat shows speed up...

\dontrun{
system.time(b2 <- bam(y ~ s(x0,bs=bs,k=k)+s(x1,bs=bs,k=k)+s(x2,bs=bs,k=k)+
       s(x3,bs=bs,k=k),data=dat,family=poisson(),cluster=cl))
}

fv <- predict(b2,cluster=cl) ## parallel prediction

if (!is.null(cl)) stopCluster(cl)
b2

## Sparse smoother example...
\dontrun{
dat <- gamSim(1,n=10000,dist="poisson",scale=.1)
system.time( b3 <- bam(y ~ te(x0,x1,bs="ps",k=10,np=FALSE)+
             s(x2,bs="ps",k=30)+s(x3,bs="ps",k=30),data=dat,
             method="REML",family=poisson(),sparse=TRUE))
b3}

}

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..
\concept{Varying coefficient model}
\concept{Functional linear model}
\concept{Penalized GLM}
\concept{Generalized Additive Model}
\concept{Penalized regression}
\concept{Spline smoothing}
\concept{Penalized regression spline}
\concept{Generalized Cross Validation}
\concept{Smoothing parameter selection}
\concept{tensor product smoothing}
\concept{thin plate spline}
\concept{P-spline}
\concept{Generalized ridge regression}






