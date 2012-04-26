\name{s}
\alias{s}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Defining smooths in GAM formulae}
\description{ Function used in definition of smooth terms within
  \code{gam} model formulae. The function does not evaluate a (spline)
  smooth - it exists purely to help set up a model using spline based smooths.
}

\usage{s(..., k=-1,fx=FALSE,bs="tp",m=NA,by=NA,xt=NULL,id=NULL,sp=NULL)}

%- maybe also `usage' for other objects documented here.
\arguments{
\item{...}{ a list of variables that are the covariates that this
    smooth is a function of.}

\item{k}{ the dimension of the basis used to represent the smooth term.
    The default depends on the number of variables that the smooth is a
    function of. \code{k} should not be less than the dimension of the
    null space of the penalty for the term (see
    \code{\link{null.space.dimension}}), but will be reset if
  it is. See \code{\link{choose.k}} for further information.}

\item{fx}{indicates whether the term is a fixed d.f. regression
  spline (\code{TRUE}) or a penalized regression spline (\code{FALSE}).}

\item{bs}{a two letter character string indicating the (penalized) smoothing basis to use.
(eg \code{"tp"} for thin plate regression spline, \code{"cr"} for cubic regression spline).
see \code{\link{smooth.terms}} for an over view of what is available.
}

\item{m}{The order of the penalty for this term (e.g. 2 for
    normal cubic spline penalty with 2nd derivatives when using 
   default t.p.r.s basis). \code{NA} signals
    autoinitialization. Only some smooth classes use this. The \code{"ps"} 
    class can use a 2 item array giving the basis and penalty order separately.}

\item{by}{a numeric or factor variable of the same dimension as each covariate. 
In the numeric vector case the elements multiply the smooth, evaluated at the corresponding 
covariate values (a `varying coefficient model' results). For the numeric \code{by} variable case the 
resulting smooth is not usually subject to a centering constraint (so the \code{by variable} should 
not be added as an additional main effect). 
In the factor \code{by} variable case a replicate of the smooth is produced for
each factor level (these smooths will be centered, so the factor usually needs to be added as 
a main effect as well). See \code{\link{gam.models}} for further details. A \code{by} variable may also be a matrix 
if covariates are matrices: in this case implements linear functional of a smooth 
(see \code{\link{gam.models}} and \code{\link{linear.functional.terms}} for details).
}

\item{xt}{Any extra information required to set up a particular basis. Used
  e.g. to set large data set handling behaviour for \code{"tp"} basis.}

\item{id}{A label or integer identifying this term in order to link its smoothing
          parameters to others of the same type. If two or more terms have the same 
          \code{id} then they will have the same smoothing paramsters, and, by default,
          the same bases (first occurance defines basis type, but data from all terms 
          used in basis construction). An \code{id} with a factor \code{by} variable causes the smooths
          at each factor level to have the same smoothing parameter.} 

\item{sp}{any supplied smoothing parameters for this term. Must be an array of the same
length as the number of penalties for this smooth. Positive or zero elements are taken as fixed 
smoothing parameters. Negative elements signal auto-initialization. Over-rides values supplied in 
\code{sp} argument to \code{\link{gam}}. Ignored by \code{gamm}.}

  }

\details{The function does not evaluate the variable arguments. To use this function to specify use of
your own smooths, note the relationships between the inputs and the output object and see the example
in \code{\link{smooth.construct}}.
}
\value{ A class \code{xx.smooth.spec} object, where \code{xx} is a basis identifying code given by
the \code{bs} argument of \code{s}. These \code{smooth.spec} objects define smooths and are turned into
bases and penalties by \code{smooth.construct} method functions. 

The returned object contains the following items:

  \item{term}{An array of text strings giving the names of the covariates that 
the term is a function of.}
  \item{bs.dim}{The dimension of the basis used to represent the smooth.}
  \item{fixed}{TRUE if the term is to be treated as a pure regression
    spline (with fixed degrees of freedom); FALSE if it is to be treated
  as a penalized regression spline}
  \item{dim}{The dimension of the smoother - i.e. the number of
    covariates that it is a function of.}
  \item{p.order}{The order of the t.p.r.s. penalty, or 0 for
    auto-selection of the penalty order.}
  \item{by}{is the name of any \code{by} variable as text (\code{"NA"} for none).}


\item{label}{A suitable text label for this smooth term.}
\item{xt}{The object passed in as argument \code{xt}.}
\item{id}{An identifying label or number for the smooth, linking it to other
smooths. Defaults to \code{NULL} for no linkage. }
\item{sp}{array of smoothing parameters for the term (negative for
auto-estimation). Defaults to \code{NULL}.}
}


\author{ Simon N. Wood \email{simon.wood@r-project.org}}


\references{

Wood, S.N. (2003) Thin plate regression splines. J.R.Statist.Soc.B 65(1):95-114

Wood S.N. (2006) Generalized Additive Models: An Introduction with R. Chapman
and Hall/CRC Press.


\url{http://www.maths.bath.ac.uk/~sw283/}

}

\seealso{ \code{\link{te}}, \code{\link{gam}}, \code{\link{gamm}}}

\examples{
# example utilising `by' variables
library(mgcv)
set.seed(0)
n<-200;sig2<-4
x1 <- runif(n, 0, 1);x2 <- runif(n, 0, 1);x3 <- runif(n, 0, 1)
fac<-c(rep(1,n/2),rep(2,n/2)) # create factor
fac.1<-rep(0,n)+(fac==1);fac.2<-1-fac.1 # and dummy variables
fac<-as.factor(fac)
f1 <-  exp(2 * x1) - 3.75887
f2 <-  0.2 * x1^11 * (10 * (1 - x1))^6 + 10 * (10 * x1)^3 * (1 - x1)^10
f<-f1*fac.1+f2*fac.2+x2
e <- rnorm(n, 0, sqrt(abs(sig2)))
y <- f + e
# NOTE: smooths will be centered, so need to include fac in model....
b<-gam(y~fac+s(x1,by=fac)+x2) 
plot(b,pages=1)
}

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..









