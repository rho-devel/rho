\name{smooth.construct.so.smooth.spec}
\alias{smooth.construct.so.smooth.spec}
\alias{smooth.construct.sf.smooth.spec}
\alias{smooth.construct.sw.smooth.spec}
\alias{soap}

%- Also NEED an `\alias' for EACH other topic documented here.
\title{Soap film smoother constructer}
\description{ Sets up basis functions and wiggliness penalties for
soap film smoothers (Wood, Bravington and Hedley, 2008). Soap film smoothers are based on the idea of
constructing a 2-D smooth as a film of soap connecting a smoothly varying
closed boundary. Unless smoothing very heavily, the film is distorted towards
the data. The smooths are designed not to smooth across boundary features (peninsulas,
for example).
 
The \code{so} version sets up the full smooth. The \code{sf} version sets up just the boundary interpolating 
soap film, while the \code{sw} version sets up the wiggly component of a soap film (zero on the boundary).
The latter two are useful for forming tensor products with soap films, and can be used with \code{\link{gamm}} and \code{gamm4}. To use these to simply set up a basis, then call via the wrapper  \code{\link{smooth.construct2}} or \code{\link{smoothCon}}.
}

\usage{
\method{smooth.construct}{so.smooth.spec}(object,data,knots)
\method{smooth.construct}{sf.smooth.spec}(object,data,knots)
\method{smooth.construct}{sw.smooth.spec}(object,data,knots)
}
%- maybe also `usage' for other objects documented here.
\arguments{
\item{object}{A smooth specification object as produced by a \code{s(...,bs="so",xt=list(bnd=bnd,...))} 
term in a \code{gam} formula. Note that the \code{xt} argument to \code{s} *must* be supplied, and 
should be a list, containing at least a boundary specification list (see details). \code{xt} may 
also contain various options controlling the boundary smooth (see details), and PDE solution grid. The dimension of the bases for boundary loops is specified via the
\code{k} argument of \code{s}, either as a single number to be used for each boundary loop, or as a 
vector of different basis dimensions for the various boundary loops.  }

\item{data}{A list or data frame containing the arguments of the smooth.}

\item{knots}{list or data frame with two named columns specifying the knot locations within 
the boundary. The column names should match the names of the arguments of the smooth. The number 
of knots defines the *interior* basis dimension (i.e. it is *not* supplied via argument \code{k} of 
\code{s}).}
}

\details{

For soap film smooths the following *must* be supplied:
\itemize{
\item{k}{ the basis dimension for each boundary loop smooth.}
\item{xt$bnd}{ the boundary specification for the smooth.}
\item{knots}{ the locations of the interior knots for the smooth.}
}
When used in a GAM then \code{k} and \code{xt} are supplied via \code{s} while \code{knots} are 
supplied in the \code{knots} argument of \code{\link{gam}}. 

The \code{bnd} element of the \code{xt} list is a list of lists (or data frames), 
specifying the loops that define the boundary. Each boundary loop list must contain 
2 columns giving the co-ordinates of points defining a boundary loop (when joined 
sequentially by line segments). Loops should not intersect (not checked). A point is 
deemed to be in the region of interest if it is interior to an odd number of boundary 
loops. Each boundary loop list may also contain a column \code{f} giving known 
boundary conditions on a loop.

The \code{bndSpec} element of \code{xt}, if non-NULL, should contain
\itemize{
\item{bs}{ the type of cyclic smoothing basis to use: one of \code{"cc"} and \code{"cp"}. 
If not \code{"cc"} then a cyclic p-spline is used, and argument 
\code{m} must be supplied.}
\item{knot.space}{ set to "even" to get even knot spacing with the "cc" basis.}
\item{m}{ 1 or 2 element array specifying order of "cp" basis and penalty.} 
}

Currently the code will not deal with more than one level of nesting of loops, or with 
separate loops without an outer enclosing loop: if there are known boundary conditions 
(identifiability constraints get awkward).

Note that the function \code{\link{locator}} provides a simple means for defining boundaries
graphically, using something like \code{bnd <-as.data.frame(locator(type="l"))}, 
after producing a plot of the domain of interest (right click to stop). If the real boundary is 
very complicated, it is probably better to use a simpler smooth boundary enclosing the true boundary, 
which represents the major boundary features that you don't want to smooth across, but doesn't follow 
every tiny detail.

Model set up, and prediction, involves evaluating basis functions which are defined as the solution to PDEs. The 
PDEs are solved numerically on a grid using sparse matrix methods, with bilinear interpolation used to obtain 
values at any location within the smoothing domain. The dimension of the PDE solution grid can be controlled 
via element \code{nmax} (default 200) of the list supplied as argument \code{xt} of \code{s} in a \code{gam} formula: it gives the number of cells to use on the longest grid side. 

A little theory: the soap film smooth \eqn{f(x,y)}{f(x,y)} is defined as the solution of
\deqn{f_{xx} + f_{yy} = g}{f_xx+f_yy = g}
subject to the condition that \eqn{f=s}{f=s}, on the boundary curve, where
\eqn{s}{s} is a smooth function (usually a cyclic penalized regression
spline). The function \eqn{g}{g} is defined as the solution of 
\deqn{g_{xx}+g_{yy}=0}{g_xx+g_yy=0}
where \eqn{g=0}{g=0} on the boundary curve and
\eqn{g(x_k,y_k)=c_k}{g(x_k,y_k)=c_k} at the `knots' of the surface; the
\eqn{c_k}{c_k} are model coefficients. 

In the simplest case, estimation of the coefficients of \eqn{f}{f} (boundary
coefficients plus \eqn{c_k}{c_k}'s) is by minimization of 
\deqn{\|z-f\|^2 + \lambda_s J_s(s) + \lambda_f J_f(f)}{||z-f||^2 + l_s J_s(s)
+ l_f J_f(f)}
where \eqn{J_s}{J_s} is usually some cubic spline type wiggliness penalty on
the boundary smooth and \eqn{J_f}{J_f} is the integral of
\eqn{(f_xx+f_yy)^2}{(f_xx+f_yy)^2} over the interior of the boundary. Both
penalties can be expressed as quadratic forms in the model coefficients. The
\eqn{\lambda}{l}'s are smoothing parameters, selectable by GCV, REML, AIC,
etc. \eqn{z}{z} represents noisy observations of \eqn{f}{f}. 
}


\value{ A list with all the elements of \code{object} plus

\item{sd}{ A list defining the PDE solution grid and domain boundary, and including the sparse LU
factorization of the PDE coefficient matrix.}

\item{X}{ The model matrix: this will have an \code{"offset"} attribute, if there are 
any known boundary conditions.}

\item{S}{ List of smoothing penalty matrices (in smallest non-zero submatrix form).}

\item{irng}{ A vector of scaling factors that have been applied to the model matrix, 
to ensure nice conditioning.}

In addition there are all the elements usually added by \code{smooth.construct} methods. 

}
\references{

Wood, S.N., M.V. Bravington and S.L. Hedley (2008) "Soap film smoothing", J.R.Statist.Soc.B 70(5), 931-955.

\url{http://www.maths.bath.ac.uk/~sw283/}

}
\section{WARNINGS }{
Soap film smooths are quite specialized, and require more setup than most smoothers (e.g. you have to supply the 
boundary and the interior knots, plus the boundary smooth basis dimension(s)). It is worth looking at the reference.
}

\author{ Simon N. Wood \email{simon.wood@r-project.org} }

\seealso{\code{\link{Predict.matrix.soap.film}}}

\examples{

require(mgcv)

##########################
## simple test function...
##########################

fsb <- list(fs.boundary())
nmax <- 100
## create some internal knots...
knots <- data.frame(v=rep(seq(-.5,3,by=.5),4),
                    w=rep(c(-.6,-.3,.3,.6),rep(8,4)))
## Simulate some fitting data, inside boundary...
n<-1000
v <- runif(n)*5-1;w<-runif(n)*2-1
y <- fs.test(v,w,b=1)
names(fsb[[1]]) <- c("v","w")
ind <- inSide(fsb,x=v,y=w) ## remove outsiders
y <- y + rnorm(n)*.3 ## add noise
y <- y[ind];v <- v[ind]; w <- w[ind] 
n <- length(y)

par(mfrow=c(3,2))
## plot boundary with knot and data locations
plot(fsb[[1]]$v,fsb[[1]]$w,type="l");points(knots,pch=20,col=2)
points(v,w,pch=".");

## Now fit the soap film smoother. 'k' is dimension of boundary smooth.
## boundary supplied in 'xt', and knots in 'knots'...
 
nmax <- 100 ## reduced from default for speed.
b <- gam(y~s(v,w,k=30,bs="so",xt=list(bnd=fsb,nmax=nmax)),knots=knots)

plot(b) ## default plot
plot(b,scheme=1)
plot(b,scheme=2)
plot(b,scheme=3)

vis.gam(b,plot.type="contour")

################################
# Fit same model in two parts...
################################

par(mfrow=c(2,2))
vis.gam(b,plot.type="contour")

b1 <- gam(y~s(v,w,k=30,bs="sf",xt=list(bnd=fsb,nmax=nmax))+
            s(v,w,k=30,bs="sw",xt=list(bnd=fsb,nmax=nmax)) ,knots=knots)
vis.gam(b,plot.type="contour")
plot(b1)
 
##################################################
## Now an example with known boundary condition...
##################################################

## Evaluate known boundary condition at boundary nodes...
fsb[[1]]$f <- fs.test(fsb[[1]]$v,fsb[[1]]$w,b=1,exclude=FALSE)

## Now fit the smooth...
bk <- gam(y~s(v,w,bs="so",xt=list(bnd=fsb,nmax=nmax)),knots=knots)
plot(bk) ## default plot

##########################################
## tensor product example (marked 
## 'Not run' to reduce CRAN checking load)
##########################################
\dontrun{
n <- 10000
v <- runif(n)*5-1;w<-runif(n)*2-1
t <- runif(n)
y <- fs.test(v,w,b=1)
y <- y + 4.2
y <- y^(.5+t)
fsb <- list(fs.boundary())
names(fsb[[1]]) <- c("v","w")
ind <- inSide(fsb,x=v,y=w) ## remove outsiders
y <- y[ind];v <- v[ind]; w <- w[ind]; t <- t[ind] 
n <- length(y)
y <- y + rnorm(n)*.05 ## add noise
knots <- data.frame(v=rep(seq(-.5,3,by=.5),4),
                    w=rep(c(-.6,-.3,.3,.6),rep(8,4)))

## notice NULL element in 'xt' list - to indicate no xt object for "cr" basis...
bk <- gam(y~ 
 te(v,w,t,bs=c("sf","cr"),k=c(25,4),d=c(2,1),xt=list(list(bnd=fsb,nmax=nmax),NULL))+
 te(v,w,t,bs=c("sw","cr"),k=c(25,4),d=c(2,1),xt=list(list(bnd=fsb,nmax=nmax),NULL))
          ,knots=knots)

par(mfrow=c(3,2))
m<-100;n<-50 
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))
tru <- matrix(fs.test(xx,yy),m,n)+4.2 ## truth

image(xm,yn,tru^.5,col=heat.colors(100),xlab="v",ylab="w",
      main="truth")
lines(fsb[[1]]$v,fsb[[1]]$w,lwd=3)
contour(xm,yn,tru^.5,add=TRUE)

vis.gam(bk,view=c("v","w"),cond=list(t=0),plot.type="contour")

image(xm,yn,tru,col=heat.colors(100),xlab="v",ylab="w",
      main="truth")
lines(fsb[[1]]$v,fsb[[1]]$w,lwd=3)
contour(xm,yn,tru,add=TRUE)

vis.gam(bk,view=c("v","w"),cond=list(t=.5),plot.type="contour")

image(xm,yn,tru^1.5,col=heat.colors(100),xlab="v",ylab="w",
      main="truth")
lines(fsb[[1]]$v,fsb[[1]]$w,lwd=3)
contour(xm,yn,tru^1.5,add=TRUE)

vis.gam(bk,view=c("v","w"),cond=list(t=1),plot.type="contour")
}

#############################
# nested boundary example...
#############################

bnd <- list(list(x=0,y=0),list(x=0,y=0))
seq(0,2*pi,length=100) -> theta
bnd[[1]]$x <- sin(theta);bnd[[1]]$y <- cos(theta)
bnd[[2]]$x <- .3 + .3*sin(theta);
bnd[[2]]$y <- .3 + .3*cos(theta)
plot(bnd[[1]]$x,bnd[[1]]$y,type="l")
lines(bnd[[2]]$x,bnd[[2]]$y)

## setup knots
k <- 8
xm <- seq(-1,1,length=k);ym <- seq(-1,1,length=k)
x=rep(xm,k);y=rep(ym,rep(k,k))
ind <- inSide(bnd,x,y)
knots <- data.frame(x=x[ind],y=y[ind])
points(knots$x,knots$y)

## a test function

f1 <- function(x,y) {
  exp(-(x-.3)^2-(y-.3)^2)
}

## plot the test function within the domain 
par(mfrow=c(2,3))
m<-100;n<-100 
xm <- seq(-1,1,length=m);yn<-seq(-1,1,length=n)
x <- rep(xm,n);y<-rep(yn,rep(m,n))
ff <- f1(x,y)
ind <- inSide(bnd,x,y)
ff[!ind] <- NA
image(xm,yn,matrix(ff,m,n),xlab="x",ylab="y")
contour(xm,yn,matrix(ff,m,n),add=TRUE)
lines(bnd[[1]]$x,bnd[[1]]$y,lwd=2);lines(bnd[[2]]$x,bnd[[2]]$y,lwd=2)

## Simulate data by noisy sampling from test function...

set.seed(1)
x <- runif(300)*2-1;y <- runif(300)*2-1
ind <- inSide(bnd,x,y)
x <- x[ind];y <- y[ind]
n <- length(x)
z <- f1(x,y) + rnorm(n)*.1

## Fit a soap film smooth to the noisy data
nmax <- 100
b <- gam(z~s(x,y,k=c(30,15),bs="so",xt=list(bnd=bnd,nmax=nmax)),knots=knots,method="REML")
plot(b) ## default plot
vis.gam(b,plot.type="contour") ## prettier version

## trying out separated fits....
ba <- gam(z~s(x,y,k=c(30,15),bs="sf",xt=list(bnd=bnd,nmax=nmax))+
          s(x,y,k=c(30,15),bs="sw",xt=list(bnd=bnd,nmax=nmax)),knots=knots,method="REML")
plot(ba)
vis.gam(ba,plot.type="contour")


}

\keyword{models} \keyword{smooth} \keyword{regression}