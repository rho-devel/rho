\name{Predict.matrix.soap.film}
\alias{Predict.matrix.soap.film}
\alias{Predict.matrix.sw}
\alias{Predict.matrix.sf}

%- Also NEED an `\alias' for EACH other topic documented here.
\title{Prediction matrix for soap film smooth}
\description{ Creates a prediction matrix for a soap film smooth object,
mapping the coefficients of the smooth to the linear predictor component for
the smooth.  This is the \code{\link{Predict.matrix}} method function required by \code{\link{gam}}.
}
\usage{
\method{Predict.matrix}{soap.film}(object,data)
\method{Predict.matrix}{sw}(object,data)
\method{Predict.matrix}{sf}(object,data)
}
%- maybe also `usage' for other objects documented here.
\arguments{

\item{object}{A class \code{"soap.film"}, \code{"sf"} or \code{"sw"} object.}

\item{data}{A list list or data frame containing the arguments of the smooth
at which predictions are required.}

}

\details{
The smooth object will be largely what is returned from
\code{\link{smooth.construct.so.smooth.spec}}, although elements \code{X} and
\code{S} are not needed, and need not be present, of course.
}


\value{ A matrix. This may have an \code{"offset"} attribute corresponding to
the contribution from any known boundary conditions on the smooth.

}
\references{

\url{http://www.maths.bath.ac.uk/~sw283/}
}
\author{ Simon N. Wood \email{s.wood@bath.ac.uk}} 

\seealso{\code{\link{smooth.construct.so.smooth.spec}}}

\examples{
## This is a lower level example. The basis and 
## penalties are obtained explicitly 
## and `magic' is used as the fitting routine...

require(mgcv)
set.seed(66)

## create a boundary...
fsb <- list(fs.boundary())

## create some internal knots...
knots <- data.frame(x=rep(seq(-.5,3,by=.5),4),
                    y=rep(c(-.6,-.3,.3,.6),rep(8,4)))

## Simulate some fitting data, inside boundary...
n<-1000
x <- runif(n)*5-1;y<-runif(n)*2-1
z <- fs.test(x,y,b=1)
ind <- inSide(fsb,x,y) ## remove outsiders
z <- z[ind];x <- x[ind]; y <- y[ind] 
n <- length(z)
z <- z + rnorm(n)*.3 ## add noise

## plot boundary with knot and data locations
plot(fsb[[1]]$x,fsb[[1]]$y,type="l");points(knots$x,knots$y,pch=20,col=2)
points(x,y,pch=".",col=3);

## set up the basis and penalties...
sob <- smooth.construct2(s(x,y,bs="so",k=40,xt=list(bnd=fsb,nmax=100)),
              data=data.frame(x=x,y=y),knots=knots)
## ... model matrix is element `X' of sob, penalties matrices 
## are in list element `S'.

## fit using `magic'
um <- magic(z,sob$X,sp=c(-1,-1),sob$S,off=c(1,1))
beta <- um$b

## produce plots...
par(mfrow=c(2,2),mar=c(4,4,1,1))
m<-100;n<-50 
xm <- seq(-1,3.5,length=m);yn<-seq(-1,1,length=n)
xx <- rep(xm,n);yy<-rep(yn,rep(m,n))

## plot truth...
tru <- matrix(fs.test(xx,yy),m,n) ## truth
image(xm,yn,tru,col=heat.colors(100),xlab="x",ylab="y")
lines(fsb[[1]]$x,fsb[[1]]$y,lwd=3)
contour(xm,yn,tru,levels=seq(-5,5,by=.25),add=TRUE)

## Plot soap, by first predicting on a fine grid...

## First get prediction matrix...
X <- Predict.matrix2(sob,data=list(x=xx,y=yy))

## Now the predictions...
fv <- X\%*\%beta

## Plot the estimated function...
image(xm,yn,matrix(fv,m,n),col=heat.colors(100),xlab="x",ylab="y")
lines(fsb[[1]]$x,fsb[[1]]$y,lwd=3)
points(x,y,pch=".")
contour(xm,yn,matrix(fv,m,n),levels=seq(-5,5,by=.25),add=TRUE)

## Plot TPRS...
b <- gam(z~s(x,y,k=100))
fv.gam <- predict(b,newdata=data.frame(x=xx,y=yy))
names(sob$sd$bnd[[1]]) <- c("xx","yy","d")
ind <- inSide(sob$sd$bnd,xx,yy)
fv.gam[!ind]<-NA
image(xm,yn,matrix(fv.gam,m,n),col=heat.colors(100),xlab="x",ylab="y")
lines(fsb[[1]]$x,fsb[[1]]$y,lwd=3)
points(x,y,pch=".")
contour(xm,yn,matrix(fv.gam,m,n),levels=seq(-5,5,by=.25),add=TRUE)

}

\keyword{models} \keyword{smooth} \keyword{regression}