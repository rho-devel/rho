\name{predict.bam}
\alias{predict.bam}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Prediction from fitted Big Additive Model model}
\description{ Essentially a wrapper for \code{\link{predict.gam}} for prediction from a 
model fitted by \code{\link{bam}}. Can compute on a parallel cluster. 

Takes a fitted \code{bam} object produced by \code{\link{bam}} 
and produces predictions given a new set of values for the model covariates 
or the original values used for the model fit. Predictions can be accompanied
by standard errors, based on the posterior distribution of the model
coefficients. The routine can optionally return the matrix by which the model
coefficients must be pre-multiplied in order to yield the values of the linear predictor at
the supplied covariate values: this is useful for obtaining credible regions
for quantities derived from the model (e.g. derivatives of smooths), and for lookup table prediction outside
\code{R} (see example code below).}

\usage{
\method{predict}{bam}(object,newdata,type="link",se.fit=FALSE,terms=NULL,
        block.size=50000,newdata.guaranteed=FALSE,na.action=na.pass,cluster=NULL,...)
}
%- maybe also `usage' for other objects documented here.
\arguments{ 
  
 \item{object}{ a fitted \code{bam} object as produced by \code{\link{bam}}.
                }
 \item{newdata}{ A data frame or list containing the values of the model covariates at which predictions
  are required. If this is not provided then predictions corresponding to the
                original data are returned. If \code{newdata} is provided then
                it should contain all the variables needed for prediction: a
                warning is generated if not. } 
 
\item{type}{ When this has the value \code{"link"} (default) the linear predictor (possibly with
associated standard errors) is returned. When \code{type="terms"} each component of the 
linear predictor is returned seperately (possibly with standard errors): this includes 
parametric model components, followed by each smooth component, but excludes
any offset and any intercept. \code{type="iterms"} is the same, except that any standard errors 
returned for smooth components will include the uncertainty about the intercept/overall mean.  When 
\code{type="response"} predictions 
on the scale of the response are returned (possibly with approximate
standard errors). When \code{type="lpmatrix"} then a matrix is returned
which yields the values of the linear predictor (minus any offset) when
postmultiplied by the
parameter vector (in this case \code{se.fit} is ignored). The latter
option is most useful for getting variance estimates for quantities derived from
the model: for example integrated quantities, or derivatives of smooths. A
linear predictor matrix can also be used to implement approximate prediction
outside \code{R} (see example code, below). }

\item{se.fit}{ when this is TRUE (not default) standard error estimates are returned for each prediction.}

\item{terms}{if \code{type=="terms"} then only results for the terms named in this array
will be returned.}

\item{block.size}{maximum number of predictions to process per call to underlying
code: larger is quicker, but more memory intensive.}

\item{newdata.guaranteed}{Set to \code{TRUE} to turn off all checking of
\code{newdata} except for sanity of factor levels: this can speed things up
for large prediction tasks, but \code{newdata} must be complete, with no
\code{NA} values for predictors required in the model. }

\item{na.action}{what to do about \code{NA} values in \code{newdata}. With the
default \code{na.pass}, any row of \code{newdata} containing \code{NA} values
for required predictors, gives rise to \code{NA} predictions (even if the term concerned has no
\code{NA} predictors). \code{na.exclude} or \code{na.omit} result in the
dropping of \code{newdata} rows, if they contain any \code{NA} values for
required predictors. If \code{newdata} is missing then \code{NA} handling is 
determined from \code{object$na.action}.}

\item{cluster}{\code{predict.bam} can compute in parallel using \link[parallel]{parLapply}
from the \code{parallel} package, if it is supplied with a cluster on which to do this (a cluster here can be some cores of a 
single machine). See details and example code for \code{\link{bam}}. 
}

\item{...}{ other arguments.}

}

\value{ If \code{type=="lpmatrix"} then a matrix is returned which will
  give a vector of linear predictor values (minus any offest) at the supplied covariate
  values, when applied to the model coefficient vector. 
Otherwise, if \code{se.fit} is \code{TRUE} then a 2 item list is returned with items (both arrays) \code{fit}
and \code{se.fit} containing predictions and associated standard error estimates, otherwise an 
array of predictions is returned. The dimensions of the returned arrays depends on whether 
\code{type} is \code{"terms"} or not: if it is then the array is 2 dimensional with each 
term in the linear predictor separate, otherwise the array is 1 dimensional and contains the 
linear predictor/predicted values (or corresponding s.e.s). The linear predictor returned termwise will 
not include the offset or the intercept.

\code{newdata} can be a data frame, list or model.frame: if it's a model frame
then all variables must be supplied.

}

\details{The standard errors produced by \code{predict.gam} are based on the
Bayesian posterior covariance matrix of the parameters \code{Vp} in the fitted
bam object. 

To facilitate plotting with \code{\link{termplot}}, if \code{object} possesses
an attribute \code{"para.only"} and \code{type=="terms"} then only parametric
terms of order 1 are returned (i.e. those that \code{termplot} can handle).

Note that, in common with other prediction functions, any offset supplied to
\code{\link{gam}} as an argument is always ignored when predicting, unlike
offsets specified in the gam model formula.

See the examples in \code{\link{predict.gam}} for how to use the \code{lpmatrix} for obtaining credible
regions for quantities derived from the model. 
}

\references{

Chambers and Hastie (1993) Statistical Models in S. Chapman & Hall.

Marra, G and S.N. Wood (2012) Coverage Properties of Confidence Intervals for Generalized Additive
Model Components. Scandinavian Journal of Statistics.

Wood S.N. (2006b) Generalized Additive Models: An Introduction with R. Chapman
and Hall/CRC Press.
}
\author{ Simon N. Wood \email{simon.wood@r-project.org} 

The design is inspired by the S function of the same name described in
Chambers and Hastie (1993) (but is not a clone).

}



\seealso{  \code{\link{bam}}, \code{\link{predict.gam}}}

\examples{
## for parallel computing see examples for ?bam

## for general useage follow examples in ?predict.gam

}
\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..

