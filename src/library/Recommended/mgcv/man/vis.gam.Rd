\name{vis.gam}
\alias{vis.gam}
\alias{persp.gam}
\title{Visualization of GAM objects}
\usage{
vis.gam(x,view=NULL,cond=list(),n.grid=30,too.far=0,col=NA,
        color="heat",contour.col=NULL,se=-1,type="link",
        plot.type="persp",zlim=NULL,nCol=50,...)
}

\arguments{
  \item{x}{a \code{gam} object, produced by \code{gam()}}

  \item{view}{an array containing the names of the two main effect terms to be displayed on the 
   x and y dimensions of the plot. If omitted the first two suitable terms
   will be used. Note that variables coerced to factors in the model formula won't work
   as view variables, and \code{vis.gam} can not detect that this has happened when setting defaults. 
}

  \item{cond}{a named list of the values to use for the other predictor terms
  (not in \code{view}). Variables omitted from this list will have the closest observed value to the median 
  for continuous variables, or the most commonly occuring level for factors. Parametric matrix variables have 
  all the entries in each column set to the observed column entry closest to the column median.
}
  \item{n.grid}{The number of grid nodes in each direction used for calculating the 
   plotted surface.} 

 \item{too.far}{ plot grid nodes that are too far from the points defined by the variables given in \code{view} 
can be excluded from the plot. \code{too.far} determines what is too far. The grid is scaled into the unit 
square along with the \code{view} variables and then grid nodes more than \code{too.far} from the predictor variables 
are excluded.}
\item{col}{The colours for the facets of the plot. If this is \code{NA} then if \code{se}>0 the facets are transparent, 
otherwise the colour scheme specified in \code{color} is used. If \code{col} is not \code{NA} then it is used as the facet 
colour.}
 \item{color}{ the colour scheme to use for plots when \code{se}<=0. One of \code{"topo"}, \code{"heat"}, \code{"cm"}, 
\code{"terrain"}, \code{"gray"} or \code{"bw"}. Schemes \code{"gray"} and
\code{"bw"} also modify the colors used when \code{se}>0.}
\item{contour.col}{sets the colour of contours when using \code{plot.type="contour"}. Default scheme used if \code{NULL}.}
\item{se}{if less than or equal to zero then only the predicted surface is plotted, but if greater than zero, then 3 
surfaces are plotted, one at the predicted values minus \code{se} standard errors, one at the predicted values and one at
the predicted values plus \code{se} standard errors.}
\item{type}{\code{"link"} to plot on linear predictor scale and \code{"response"} to plot on the response scale.}
\item{plot.type}{one of \code{"contour"} or \code{"persp"}.}
\item{zlim}{a two item array giving the lower and upper limits for the z-axis
scale. \code{NULL} to choose automatically.}
\item{nCol}{The number of colors to use in color schemes.}

\item{...}{other options to pass on to \code{\link{persp}},
\code{\link{image}} or \code{\link{contour}}. In particular \code{ticktype="detailed"} will add proper axes 
labelling to the plots. }
}
\value{Simply produces a plot.}

\description{ Produces perspective or contour plot views of \code{gam} model
predictions, fixing all but the values in \code{view} to the  values supplied in \code{cond}. 
}
\details{ The x and y limits are determined by the ranges of the terms named in \code{view}. If \code{se}<=0 then 
a single (height colour coded, by default) surface is produced, otherwise three (by default see-through) meshes are produced at 
mean and +/- \code{se} standard errors. Parts of the x-y plane too far from
data can be excluded by setting \code{too.far}

All options to the underlying graphics functions can be reset by passing them
as extra arguments \code{...}: such supplied values will always over-ride the
default values used by \code{vis.gam}. 
}

\author{Simon Wood \email{simon.wood@r-project.org}

 Based on an original idea and design by Mike Lonergan.}

\section{WARNINGS}{
The routine can not detect that a variable has been coerced to factor within a model formula, 
and will therefore fail if such a variable is used as a \code{view} variable. When setting 
default \code{view} variables it can not detect this situation either, which can cause failures
if the coerced variables are the first, otherwise suitable, variables encountered.
}

\seealso{
\code{\link{persp}} and \code{\link{gam}}.
}
\examples{
library(mgcv)
set.seed(0)
n<-200;sig2<-4
x0 <- runif(n, 0, 1);x1 <- runif(n, 0, 1)
x2 <- runif(n, 0, 1)
y<-x0^2+x1*x2 +runif(n,-0.3,0.3)
g<-gam(y~s(x0,x1,x2))
old.par<-par(mfrow=c(2,2))
# display the prediction surface in x0, x1 ....
vis.gam(g,ticktype="detailed",color="heat",theta=-35)  
vis.gam(g,se=2,theta=-35) # with twice standard error surfaces
vis.gam(g, view=c("x1","x2"),cond=list(x0=0.75)) # different view 
vis.gam(g, view=c("x1","x2"),cond=list(x0=.75),theta=210,phi=40,
        too.far=.07)
# ..... areas where there is no data are not plotted

# contour examples....
vis.gam(g, view=c("x1","x2"),plot.type="contour",color="heat")
vis.gam(g, view=c("x1","x2"),plot.type="contour",color="terrain")
vis.gam(g, view=c("x1","x2"),plot.type="contour",color="topo")
vis.gam(g, view=c("x1","x2"),plot.type="contour",color="cm")


par(old.par)

# Examples with factor and "by" variables

fac<-rep(1:4,20)
x<-runif(80)
y<-fac+2*x^2+rnorm(80)*0.1
fac<-factor(fac)
b<-gam(y~fac+s(x))

vis.gam(b,theta=-35,color="heat") # factor example

z<-rnorm(80)*0.4   
y<-as.numeric(fac)+3*x^2*z+rnorm(80)*0.1
b<-gam(y~fac+s(x,by=z))

vis.gam(b,theta=-35,color="heat",cond=list(z=1)) # by variable example

vis.gam(b,view=c("z","x"),theta= 35) # plot against by variable

}
\keyword{hplot} \keyword{models} \keyword{smooth} \keyword{regression}
