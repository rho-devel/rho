\name{concurvity}
\alias{concurvity}

%- Also NEED an `\alias' for EACH other topic documented here.
\title{GAM concurvity measures}

\description{Produces summary measures of concurvity between \code{\link{gam}} components.
}
\usage{
concurvity(b,full=TRUE)
}
%- maybe also `usage' for other objects documented here.

\arguments{ 
\item{b}{An object inheriting from class \code{"gam"}.}
\item{full}{If \code{TRUE} then concurvity of each term with the whole of the rest of the model is considered. 
If \code{FALSE} then pairwise concurvity measures between each smooth term (as well as the parametric component) 
are considered.}
} 


\value{ If \code{full=TRUE} a matrix with one column for each term and one row for each of the 3 concurvity measures detailed below. If \code{full=FALSE} a list of 3 matrices, one for each of the three concurvity measures detailed below. Each row of 
the matrix relates to how the model terms depend on the model term supplying that rows name.
}


\details{
Concurvity occurs when some smooth term in a model could be approximated by one or more of the other smooth terms 
in the model. This is often the case when a smooth of space is included in a model, along with smooths of other covariates 
that also vary more or less smoothly in space. Similarly it tends to be an issue in models including a smooth of time, 
along with smooths of other time varying covariates.

Concurvity can be viewed as a generalization of co-linearity, and causes similar problems of interpretation. It can also make estimates somewhat unstable (so that they become sensitive to apparently innocuous modelling details, for example). 

This routine computes three related indices of concurvity, all bounded between 0 and 1, with 0 indicating no problem, 
and 1 indicating total lack of identifiability. The three indices are all based on the idea that a smooth term, f,  
in the model can be decomposed into a part, g, that lies entirely in the space of one or more other terms 
in the model, and a remainder part that is completely within the term's own space. If g makes up a large part of f then there is a concurvity problem. The indices used are all based on the square of ||g||/||f||, that is the ratio of the squared 
Euclidean norms of the vectors of f and g evaluated at the observed covariate values. 

The three measures are as follows 
\describe{
\item{worst}{This is the largest value that the square of ||g||/||f|| could take for any coefficient vector. This is a fairly pessimistic measure, as it looks at the worst case irrespective of data. This is the only measure that is symmetric.}

\item{observed}{This just returns the value of the square of ||g||/||f|| according to the estimated coefficients. 
This could be a bit over-optimistic about the potential for a problem in some cases. }

\item{estimate}{This is the squared F-norm of the basis for g divided by the F-norm of the basis for f. 
It is a measure of the 
extent to which the f basis can be explained by the g basis. It does not suffer from the pessimism or potential for 
over-optimism of the previous two measures, but is less easy to understand.}

}


}


\references{


\url{http://www.maths.bath.ac.uk/~sw283/}
}
\author{ Simon N. Wood \email{simon.wood@r-project.org}


}


\examples{
library(mgcv)
## simulate data with concurvity...
set.seed(8);n<- 200
f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 *
            (10 * x)^3 * (1 - x)^10
t <- sort(runif(n)) ## first covariate
## make covariate x a smooth function of t + noise...
x <- f2(t) + rnorm(n)*3
## simulate response dependent on t and x...
y <- sin(4*pi*t) + exp(x/20) + rnorm(n)*.3

## fit model...
b <- gam(y ~ s(t,k=15) + s(x,k=15),method="REML")

## assess concurvity between each term and `rest of model'...
concurvity(b)

## ... and now look at pairwise concurvity between terms...
concurvity(b,full=FALSE)

}








