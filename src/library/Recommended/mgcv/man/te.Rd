\name{te}
\alias{te}\alias{ti}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Define tensor product smooths or tensor product interactions in GAM formulae}
\description{ Functions used for the definition of tensor product smooths and interactions within
  \code{gam} model formulae. \code{te} produces a full tensor product smooth, while \code{ti} 
  produces a tensor product interaction, appropriate when the main effects (and any lower 
  interactions) are also present.

 The functions do not evaluate the
  smooth - they exists purely to help set up a model using tensor product 
  based smooths. Designed to construct tensor products from any marginal
  smooths with a basis-penalty representation (with the restriction that each 
  marginal smooth must have only one penalty).
}

\usage{te(..., k=NA,bs="cr",m=NA,d=NA,by=NA,fx=FALSE,
              mp=TRUE,np=TRUE,xt=NULL,id=NULL,sp=NULL)
ti(..., k=NA,bs="cr",m=NA,d=NA,by=NA,fx=FALSE,
              np=TRUE,xt=NULL,id=NULL,sp=NULL)
}
%- maybe also `usage' for other objects documented here.
\arguments{
\item{...}{ a list of variables that are the covariates that this
    smooth is a function of.}

\item{k}{ the dimension(s) of the bases used to represent the smooth term.
    If not supplied then set to \code{5^d}. If supplied as a single number then this 
    basis dimension is used for each basis. If supplied as an array then the elements are
    the dimensions of the component (marginal) bases of the tensor
    product. See \code{\link{choose.k}} for further information.}

\item{bs}{array (or single character string) specifying the type for each 
marginal basis. \code{"cr"} for cubic regression spline; \code{"cs"} for cubic
regression spline with shrinkage; \code{"cc"} for periodic/cyclic 
cubic regression spline; \code{"tp"} for thin plate regression spline;
\code{"ts"} for t.p.r.s. with extra shrinkage. See \code{\link{smooth.terms}} for details 
and full list. User defined bases can 
also be used here (see \code{\link{smooth.construct}} for an example). If only one 
basis code is given then this is used for all bases.}

\item{m}{The order of the spline and its penalty (for smooth classes that use this) for each term. 
   If a single number is given  then it is used for all terms. A vector can be used to 
  supply a different \code{m} for each margin. For marginals that take vector \code{m} 
  (e.g. \code{\link{p.spline}} and \code{\link{Duchon.spline}}), then
  a list can be supplied, with a vector element for each margin. \code{NA} autoinitializes. 
  \code{m} is ignored by some bases (e.g. \code{"cr"}).}

\item{d}{array of marginal basis dimensions. For example if you want a smooth for 3 covariates 
made up of a tensor product of a 2 dimensional t.p.r.s. basis and a 1-dimensional basis, then 
set \code{d=c(2,1)}. Incompatibilities between built in basis types and dimension will be
resolved by resetting the basis type.}

\item{by}{a numeric or factor variable of the same dimension as each covariate. 
In the numeric vector case the elements multiply the smooth evaluated at the corresponding 
covariate values (a `varying coefficient model' results). 
In the factor case causes a replicate of the smooth to be produced for
each factor level. See \code{\link{gam.models}} for further details. May also be a matrix 
if covariates are matrices: in this case implements linear functional of a smooth 
(see \code{\link{gam.models}} and \code{\link{linear.functional.terms}} for details).}

\item{fx}{indicates whether the term is a fixed d.f. regression
  spline (\code{TRUE}) or a penalized regression spline (\code{FALSE}).}

\item{mp}{\code{TRUE} to use multiple penalties for the smooth. \code{FALSE} to use only 
a single penalty: single penalties are not recommended and are deprecated - they tend to allow only rather 
wiggly models.}

\item{np}{ \code{TRUE} to use the `normal parameterization' for a tensor
product smooth. This represents any 1-d marginal smooths
via parameters that are function values at `knots',
spread evenly through the data. The parameterization makes the penalties
easily interpretable, however it can reduce numerical stability in some cases.}

\item{xt}{Either a single object, providing any extra information to be passed
to each marginal basis constructor, or a list of such objects, one for each
marginal basis. }

\item{id}{A label or integer identifying this term in order to link its smoothing
          parameters to others of the same type. If two or more smooth terms have the same 
          \code{id} then they will have the same smoothing paramsters, and, by default,
          the same bases (first occurance defines basis type, but data from all terms 
          used in basis construction).}

\item{sp}{any supplied smoothing parameters for this term. Must be an array of the same
length as the number of penalties for this smooth. Positive or zero elements are taken as fixed 
smoothing parameters. Negative elements signal auto-initialization. Over-rides values supplied in 
\code{sp} argument to \code{\link{gam}}. Ignored by \code{gamm}.}


}

\details{ Smooths of several covariates can be constructed from tensor products of the bases
used to represent smooths of one (or sometimes more) of the covariates. To do this `marginal' bases
are produced with associated model matrices and penalty matrices, and these are then combined in the
manner described in \code{\link{tensor.prod.model.matrix}} and \code{\link{tensor.prod.penalties}}, to produce 
a single model matrix for the smooth, but multiple penalties (one for each marginal basis). The basis dimension 
of the whole smooth is the product of the basis dimensions of the marginal smooths.
 
An option for operating with a single penalty (The Kronecker product of the marginal penalties) is provided, but 
it is rarely of practical use, and is deprecated: the penalty is typically so rank deficient that 
even the smoothest resulting model will have rather high estimated degrees of freedom. 

Tensor product smooths are especially useful for representing functions of covariates measured in different units, 
although they are typically not quite as nicely behaved as t.p.r.s. smooths for well scaled covariates.

It is sometimes useful to investigate smooth models with a main-effects + interactions structure, for example
\deqn{f_1(x)  + f_2(z) + f_3(x,z)}{f_1(x)  + f_2(z) + f_3(x,z)}
This functional ANOVA decomposition is supported by \code{ti} terms, which produce tensor product interactions from which the main effects have been excluded, under the assumption that they will be included separately. For example the \code{~ ti(x) + ti(z) + ti(x,z)} would produce the above main effects + interaction structure. This is much better than attempting the same thing with \code{s}or \code{te} terms representing the interactions (although mgcv does not forbid it). Technically \code{ti} terms are very simple: they simply construct tensor product bases from marginal smooths to which identifiability constraints (usually sum-to-zero) have already been applied: correct nesting is then automatic (as with all interactions in a GLM framework). 

The `normal parameterization' (\code{np=TRUE}) re-parameterizes the marginal
smooths of a tensor product smooth so that the parameters are function values
at a set of points spread evenly through the range of values of the covariate
of the smooth. This means that the penalty of the tensor product associated
with any particular covariate direction can be interpreted as the penalty of
the appropriate marginal smooth applied in that direction and averaged over
the smooth. Currently this is only done for marginals of a single
variable. This parameterization can reduce numerical stability  when used
with marginal smooths other than \code{"cc"}, \code{"cr"} and \code{"cs"}: if
this causes problems, set \code{np=FALSE}.

Note that tensor product smooths should not be centred (have identifiability constraints imposed) 
if any marginals would not need centering. The constructor for tensor product smooths 
ensures that this happens.

The function does not evaluate the variable arguments.

}
\value{ A class \code{tensor.smooth.spec} object defining a tensor product smooth
 to be turned into a basis and penalties by the \code{smooth.construct.tensor.smooth.spec} function. 

The returned object contains the following items:

\item{margin}{A list of \code{smooth.spec} objects of the type returned by \code{\link{s}}, 
defining the basis from which the tensor product smooth is constructed.}

\item{term}{An array of text strings giving the names of the covariates that 
the term is a function of.}

\item{by}{is the name of any \code{by} variable as text (\code{"NA"} for none).}

\item{fx}{ logical array with element for each penalty of the term
(tensor product smooths have multiple penalties). \code{TRUE} if the penalty is to 
be ignored, \code{FALSE}, otherwise. }
  

\item{label}{A suitable text label for this smooth term.}

\item{dim}{The dimension of the smoother - i.e. the number of
    covariates that it is a function of.}

\item{mp}{\code{TRUE} is multiple penalties are to be used (default).}

\item{np}{\code{TRUE} to re-parameterize 1-D marginal smooths in terms of function
values (defualt).}

\item{id}{the \code{id} argument supplied to \code{te}.}

\item{sp}{the \code{sp} argument supplied to \code{te}.}

\item{inter}{\code{TRUE} if the term was generated by \code{ti}, \code{FALSE} otherwise.}
}


\author{ Simon N. Wood \email{simon.wood@r-project.org}}
\references{

Wood, S.N. (2006a) Low rank scale invariant tensor product smooths for
generalized additive mixed models. Biometrics 62(4):1025-1036


\url{http://www.maths.bath.ac.uk/~sw283/}

}

\seealso{ \code{\link{s}},\code{\link{gam}},\code{\link{gamm}}, 
\code{\link{smooth.construct.tensor.smooth.spec}}}

\examples{

# following shows how tensor pruduct deals nicely with 
# badly scaled covariates (range of x 5\% of range of z )
require(mgcv)
test1<-function(x,z,sx=0.3,sz=0.4)  
{ x<-x*20
  (pi**sx*sz)*(1.2*exp(-(x-0.2)^2/sx^2-(z-0.3)^2/sz^2)+
  0.8*exp(-(x-0.7)^2/sx^2-(z-0.8)^2/sz^2))
}
n<-500
old.par<-par(mfrow=c(2,2))
x<-runif(n)/20;z<-runif(n);
xs<-seq(0,1,length=30)/20;zs<-seq(0,1,length=30)
pr<-data.frame(x=rep(xs,30),z=rep(zs,rep(30,30)))
truth<-matrix(test1(pr$x,pr$z),30,30)
f <- test1(x,z)
y <- f + rnorm(n)*0.2
b1<-gam(y~s(x,z))
persp(xs,zs,truth);title("truth")
vis.gam(b1);title("t.p.r.s")
b2<-gam(y~te(x,z))
vis.gam(b2);title("tensor product")
b3<-gam(y~ ti(x) + ti(z) + ti(x,z))
vis.gam(b3);title("tensor anova")
par(old.par)

test2<-function(u,v,w,sv=0.3,sw=0.4)  
{ ((pi**sv*sw)*(1.2*exp(-(v-0.2)^2/sv^2-(w-0.3)^2/sw^2)+
  0.8*exp(-(v-0.7)^2/sv^2-(w-0.8)^2/sw^2)))*(u-0.5)^2*20
}
n <- 500
v <- runif(n);w<-runif(n);u<-runif(n)
f <- test2(u,v,w)
y <- f + rnorm(n)*0.2
# tensor product of 2D Duchon spline and 1D cr spline
m <- list(c(1,.5),rep(0,0)) ## example of list form of m
b <- gam(y~te(v,w,u,k=c(30,5),d=c(2,1),bs=c("ds","cr"),m=m))
op <- par(mfrow=c(2,2))
vis.gam(b,cond=list(u=0),color="heat",zlim=c(-0.2,3.5))
vis.gam(b,cond=list(u=.33),color="heat",zlim=c(-0.2,3.5))
vis.gam(b,cond=list(u=.67),color="heat",zlim=c(-0.2,3.5))
vis.gam(b,cond=list(u=1),color="heat",zlim=c(-0.2,3.5))
par(op)

}

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..









