\name{pcls}
\alias{pcls}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Penalized Constrained Least Squares Fitting}
\description{
Solves least squares problems with quadratic penalties subject to linear
equality and inequality constraints using quadratic programming.
}
\usage{
pcls(M)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{M}{is the single list argument to \code{pcls}. It should have  the 
  following elements:
\describe{
 \item{y}{The response data vector.}
 \item{w}{A vector of weights for the data (often proportional to the 
           reciprocal of the variance). }
 \item{X}{The design matrix for the problem, note that \code{ncol(M$X)}
            must give the number of model parameters, while \code{nrow(M$X)} 
            should give the number of data.}
\item{C}{Matrix containing any linear equality constraints 
            on the problem (e.g. \eqn{ \bf C}{C} in \eqn{ {\bf Cp}={\bf
		c} }{Cp=c}). If you have no equality constraints
	    initialize this to a zero by zero matrix. Note that there is no need 
            to supply the vector \eqn{ \bf c}{c}, it is defined implicitly by the 
            initial parameter estimates \eqn{ \bf p}{p}.}
 \item{S}{ A list of penalty matrices. \code{S[[i]]} is the smallest contiguous matrix including 
          all the non-zero elements of the ith penalty matrix. The first parameter it
          penalizes is given by \code{off[i]+1} (starting counting at 1). }
 \item{off}{ Offset values locating the elements of \code{M$S} in
   the correct location within each penalty coefficient matrix. (Zero
   offset implies starting in first location)}
\item{sp}{ An array of  smoothing parameter estimates.}
\item{p}{An array of feasible initial parameter estimates - these must
satisfy the constraints, but should avoid satisfying the inequality
constraints as equality constraints.}
\item{Ain}{Matrix for the inequality constraints \eqn{ {\bf A}_{in}
    {\bf p} > {\bf b}_{in}}{A_in p > b}. }
\item{bin}{vector in the inequality constraints. }
} % end describe
} % end M
}
\details{ 

  This solves the problem:
 
\deqn{ minimise~ \| { \bf W}^{1/2} ({ \bf Xp - y} ) \|^2  +  \sum_{i=1}^m
\lambda_i {\bf p^\prime S}_i{\bf p} }{ min || W^0.5 (Xp-y) ||^2 + 
lambda_1 p'S_1 p + lambda_1 p'S_2 p + . . .}
subject to constraints \eqn{ {\bf Cp}={\bf c}}{Cp=c} and \eqn{ {\bf
    A}_{in}{\bf p}>{\bf b}_{in}}{A_in p > b_in}, w.r.t. \eqn{\bf p}{p} given the
smoothing parameters \eqn{\lambda_i}{lambda_i}.
\eqn{ {\bf X}}{X} is a design matrix, \eqn{\bf p}{p} a parameter vector, 
\eqn{\bf y}{y} a data vector, \eqn{\bf W}{W} a diagonal weight matrix,
\eqn{ {\bf S}_i}{S_i} a positive semi-definite matrix  of coefficients
defining the ith penalty and \eqn{\bf C}{C} a matrix of coefficients 
defining the linear equality constraints on the problem. The smoothing
parameters are the \eqn{\lambda_i}{lambda_i}. Note that \eqn{ {\bf X}}{X}
must be of full column rank, at least when projected  into the null space
of any equality constraints. \eqn{ {\bf A}_{in}}{A_in} is a matrix of
coefficients defining the inequality constraints, while \eqn{ {\bf
    b}_{in}}{b_in} is a vector involved in defining the inequality constraints.  

Quadratic programming is used to perform the solution. The method used
is designed for maximum stability with least squares problems:
i.e. \eqn{ {\bf X}^\prime {\bf X}}{X'X} is not formed explicitly. See
Gill et al. 1981.

}
\value{ The function returns an array containing the estimated parameter
  vector. 
   
}
\references{

Gill, P.E., Murray, W. and Wright, M.H. (1981) Practical Optimization. Academic
Press, London. 

Wood, S.N. (1994) Monotonic smoothing splines fitted by cross validation SIAM
Journal on Scientific Computing 15(5):1126-1133

\url{http://www.maths.bath.ac.uk/~sw283/}
}
\author{ Simon N. Wood \email{simon.wood@r-project.org}} 

\seealso{  \code{\link{magic}}, \code{\link{mono.con}}  }

\examples{
require(mgcv)
# first an un-penalized example - fit E(y)=a+bx subject to a>0
set.seed(0)
n<-100
x<-runif(n);y<-x-0.2+rnorm(n)*0.1
M<-list(X=matrix(0,n,2),p=c(0.1,0.5),off=array(0,0),S=list(),
Ain=matrix(0,1,2),bin=0,C=matrix(0,0,0),sp=array(0,0),y=y,w=y*0+1)
M$X[,1]<-1;M$X[,2]<-x;M$Ain[1,]<-c(1,0)
pcls(M)->M$p
plot(x,y);abline(M$p,col=2);abline(coef(lm(y~x)),col=3)

# Penalized example: monotonic penalized regression spline .....

# Generate data from a monotonic truth.
x<-runif(100)*4-1;x<-sort(x);
f<-exp(4*x)/(1+exp(4*x));y<-f+rnorm(100)*0.1;plot(x,y)
dat<-data.frame(x=x,y=y)
# Show regular spline fit (and save fitted object)
f.ug<-gam(y~s(x,k=10,bs="cr"));lines(x,fitted(f.ug))
# Create Design matrix, constraints etc. for monotonic spline....
sm<-smoothCon(s(x,k=10,bs="cr"),dat,knots=NULL)[[1]]
F<-mono.con(sm$xp);   # get constraints
G<-list(X=sm$X,C=matrix(0,0,0),sp=f.ug$sp,p=sm$xp,y=y,w=y*0+1)
G$Ain<-F$A;G$bin<-F$b;G$S<-sm$S;G$off<-0

p<-pcls(G);  # fit spline (using s.p. from unconstrained fit)

fv<-Predict.matrix(sm,data.frame(x=x))\%*\%p
lines(x,fv,col=2)

# now a tprs example of the same thing....

f.ug<-gam(y~s(x,k=10));lines(x,fitted(f.ug))
# Create Design matrix, constriants etc. for monotonic spline....
sm<-smoothCon(s(x,k=10,bs="tp"),dat,knots=NULL)[[1]]
xc<-0:39/39 # points on [0,1]  
nc<-length(xc)  # number of constraints
xc<-xc*4-1  # points at which to impose constraints
A0<-Predict.matrix(sm,data.frame(x=xc)) 
# ... A0%*%p evaluates spline at xc points
A1<-Predict.matrix(sm,data.frame(x=xc+1e-6)) 
A<-(A1-A0)/1e-6    
##  ... approx. constraint matrix (A\%*\%p is -ve 
## spline gradient at points xc)
G<-list(X=sm$X,C=matrix(0,0,0),sp=f.ug$sp,y=y,w=y*0+1,S=sm$S,off=0)
G$Ain<-A;    # constraint matrix
G$bin<-rep(0,nc);  # constraint vector
G$p<-rep(0,10);G$p[10]<-0.1  
# ... monotonic start params, got by setting coefs of polynomial part
p<-pcls(G);  # fit spline (using s.p. from unconstrained fit)

fv2<-Predict.matrix(sm,data.frame(x=x))\%*\%p
lines(x,fv2,col=3)

######################################
## monotonic additive model example...
######################################

## First simulate data...

set.seed(10)
f1 <- function(x) 5*exp(4*x)/(1+exp(4*x));
f2 <- function(x) {
  ind <- x > .5
  f <- x*0
  f[ind] <- (x[ind] - .5)^2*10
  f 
}
f3 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 
      10 * (10 * x)^3 * (1 - x)^10
n <- 200
x <- runif(n); z <- runif(n); v <- runif(n)
mu <- f1(x) + f2(z) + f3(v)
y <- mu + rnorm(n)

## Preliminary unconstrained gam fit...
G <- gam(y~s(x)+s(z)+s(v,k=20),fit=FALSE)
b <- gam(G=G)

## generate constraints, by finite differencing
## using predict.gam ....
eps <- 1e-7
pd0 <- data.frame(x=seq(0,1,length=100),z=rep(.5,100),
                  v=rep(.5,100))
pd1 <- data.frame(x=seq(0,1,length=100)+eps,z=rep(.5,100),
                  v=rep(.5,100))
X0 <- predict(b,newdata=pd0,type="lpmatrix")
X1 <- predict(b,newdata=pd1,type="lpmatrix")
Xx <- (X1 - X0)/eps ## Xx \%*\% coef(b) must be positive 
pd0 <- data.frame(z=seq(0,1,length=100),x=rep(.5,100),
                  v=rep(.5,100))
pd1 <- data.frame(z=seq(0,1,length=100)+eps,x=rep(.5,100),
                  v=rep(.5,100))
X0 <- predict(b,newdata=pd0,type="lpmatrix")
X1 <- predict(b,newdata=pd1,type="lpmatrix")
Xz <- (X1-X0)/eps
G$Ain <- rbind(Xx,Xz) ## inequality constraint matrix
G$bin <- rep(0,nrow(G$Ain))
G$sp <- b$sp
G$p <- coef(b)
G$off <- G$off-1 ## to match what pcls is expecting
## force inital parameters to meet constraint
G$p[11:18] <- G$p[2:9]<- 0
p <- pcls(G) ## constrained fit
par(mfrow=c(2,3))
plot(b) ## original fit
b$coefficients <- p
plot(b) ## constrained fit
## note that standard errors in preceding plot are obtained from
## unconstrained fit

}
\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..




