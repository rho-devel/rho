\name{t2}
\alias{t2}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Define alternative tensor product smooths in GAM formulae}
\description{ Alternative to \code{\link{te}} for defining tensor product smooths
in a \code{\link{gam}} formula. Results in a construction in which the penalties are 
non-overlapping multiples of identity matrices (with some rows and columns zeroed). 
The construction, which is due to Fabian Scheipl, is analogous to Smoothing Spline ANOVA 
(Gu, 2002), but using low rank penalized regression spline marginals. The main advantage of this construction 
is that it is useable with \code{gamm4} from package \code{gamm4}.
}

\usage{t2(..., k=NA,bs="cr",m=NA,d=NA,by=NA,xt=NULL,
               id=NULL,sp=NULL,full=FALSE,ord=NULL)
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

\item{full}{If \code{TRUE} then there is a separate penalty for each combination of null space column 
and range space. This gives strict invariance. If \code{FALSE} each combination of null space and 
range space generates one penalty, but the coulmns of each null space basis are treated as one group. 
The latter is more parsimonious, but does mean that invariance is only 
achieved by an arbitrary rescaling of null space basis vectors.}

\item{ord}{an array giving the orders of terms to retain. Here order means number of marginal range spaces
used in the construction of the component. \code{NULL} to retain everything. }
}

\details{ Smooths of several covariates can be constructed from tensor products of the bases
used to represent smooths of one (or sometimes more) of the covariates. To do this `marginal' bases
are produced with associated model matrices and penalty matrices. These are reparameterized so that the 
penalty is zero everywhere, except for some elements on the leading diagonal, which all have the same non-zero value. 
This reparameterization results in an unpenalized and a penalized subset of parameters, for each marginal basis (see 
e.g. appendix of Wood, 2004, for details).

The re-parameterized marginal bases are then combined to produce a basis for a single function of all the covariates 
(dimension given by the product of the dimensions of the marginal bases). In this set up there are multiple 
penalty matrices --- all zero, but for a mixture of a constant and zeros on the leading diagonal. No two penalties have 
a non-zero entry in the same place. 

Essentially the basis for the tensor product can be thought of as being constructed from a set of
products of the penalized (range) or unpenalized (null) space bases of the marginal smooths  (see Gu, 2002, section 2.4). 
To construct one of the set, choose either the 
null space or the range space from each marginal, and from these bases construct a product basis. The result is subject to a ridge 
penalty (unless it happens to be a product entirely of marginal null spaces). The whole basis for the smooth is constructed from 
all the different product bases that can be constructed in this way. The separately penalized components of the smooth basis each
have an interpretation in terms of the ANOVA - decomposition of the term. 
See \code{\link{pen.edf}} for some further information.

Note that there are two ways to construct the product. When \code{full=FALSE} then the null space bases are treated as a whole in each product,
but when \code{full=TRUE} each null space column is treated as a separate null space. The latter results in more penalties, but is the strict 
analog of the SS-ANOVA approach.

Tensor product smooths are especially useful for representing functions of covariates measured in different units, 
although they are typically not quite as nicely behaved as t.p.r.s. smooths for well scaled covariates.

Note also that GAMs constructed from lower rank tensor product smooths are
nested within GAMs constructed from higher rank tensor product smooths if the
same marginal bases are used in both cases (the marginal smooths themselves
are just special cases of tensor product smooths.)

Note that tensor product smooths should not be centred (have identifiability constraints imposed) 
if any marginals would not need centering. The constructor for tensor product smooths 
ensures that this happens.

The function does not evaluate the variable arguments.

}
\value{ A class \code{t2.smooth.spec} object defining a tensor product smooth
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
}


\author{ Simon N. Wood \email{simon.wood@r-project.org} and Fabian Scheipl}
\references{

Wood S.N., F. Scheipl and J.J. Faraway (2012) Straightforward intermediate rank tensor product smoothing
in mixed models. Statistical Computing.

Gu, C. (2002) Smoothing Spline ANOVA, Springer.

\url{http://people.bath.ac.uk/sw283/}

Alternative approaches to functional ANOVA decompositions, 
*not* implemented by t2 terms, are discussed in:

Belitz and Lang (2008) Simultaneous selection of variables and smoothing parameters in structured additive regression models. Computational Statistics & Data Analysis, 53(1):61-81

Lee, D-J and M. Durban (2011) P-spline ANOVA type interaction models for spatio-temporal smoothing. Statistical Modelling
 
Wood, S.N. (2006) Low-Rank Scale-Invariant Tensor Product Smooths for Generalized Additive Mixed Models. Biometrics 62(4): 1025-1036.

}

\seealso{\code{\link{te}} \code{\link{s}},\code{\link{gam}},\code{\link{gamm}}, 
}

\examples{

# following shows how tensor product deals nicely with 
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
b2<-gam(y~t2(x,z))
vis.gam(b2);title("tensor product")
b3<-gam(y~t2(x,z,bs=c("tp","tp")))
vis.gam(b3);title("tensor product")
par(old.par)

test2<-function(u,v,w,sv=0.3,sw=0.4)  
{ ((pi**sv*sw)*(1.2*exp(-(v-0.2)^2/sv^2-(w-0.3)^2/sw^2)+
  0.8*exp(-(v-0.7)^2/sv^2-(w-0.8)^2/sw^2)))*(u-0.5)^2*20
}
n <- 500
v <- runif(n);w<-runif(n);u<-runif(n)
f <- test2(u,v,w)
y <- f + rnorm(n)*0.2

## tensor product of 2D Duchon spline and 1D cr spline
m <- list(c(1,.5),0)
b <- gam(y~t2(v,w,u,k=c(30,5),d=c(2,1),bs=c("ds","cr"),m=m))

## look at the edf per penalty. "rr" denotes interaction term 
## (range space range space). "rn" is interaction of null space
## for u with range space for v,w...
pen.edf(b) 

## plot results...
op <- par(mfrow=c(2,2))
vis.gam(b,cond=list(u=0),color="heat",zlim=c(-0.2,3.5))
vis.gam(b,cond=list(u=.33),color="heat",zlim=c(-0.2,3.5))
vis.gam(b,cond=list(u=.67),color="heat",zlim=c(-0.2,3.5))
vis.gam(b,cond=list(u=1),color="heat",zlim=c(-0.2,3.5))
par(op)

b <- gam(y~t2(v,w,u,k=c(30,5),d=c(2,1),bs=c("tp","cr"),full=TRUE),
         method="ML")
## more penalties now. numbers in labels like "r1" indicate which 
## basis function of a null space is involved in the term. 
pen.edf(b) 

}

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..









