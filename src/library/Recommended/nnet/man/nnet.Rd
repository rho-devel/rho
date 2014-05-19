% file nnet/man/nnet.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{nnet}
\alias{nnet}
\alias{nnet.default}
\alias{nnet.formula}
\alias{add.net}
\alias{norm.net}
\alias{eval.nn}
\alias{coef.nnet}
\alias{print.nnet}
% \alias{residuals.nnet}
\alias{summary.nnet}
\alias{print.summary.nnet}
\title{
Fit Neural Networks
}
\description{
Fit single-hidden-layer neural network, possibly with skip-layer connections.
}
\usage{
nnet(x, \dots)

\method{nnet}{formula}(formula, data, weights, \dots,
     subset, na.action, contrasts = NULL)

\method{nnet}{default}(x, y, weights, size, Wts, mask,
     linout = FALSE, entropy = FALSE, softmax = FALSE,
     censored = FALSE, skip = FALSE, rang = 0.7, decay = 0,
     maxit = 100, Hess = FALSE, trace = TRUE, MaxNWts = 1000,
     abstol = 1.0e-4, reltol = 1.0e-8, \dots)
}
\arguments{
\item{formula}{
A formula of the form \code{class ~ x1 + x2 + \dots}
}
\item{x}{
matrix or data frame of \code{x} values for examples.
}
\item{y}{
matrix or data frame of target values for examples.
}
\item{weights}{
(case) weights for each example -- if missing defaults to 1.
}
\item{size}{
number of units in the hidden layer. Can be zero if there are skip-layer units.
}
\item{data}{
Data frame from which variables specified in  \code{formula} are
preferentially to be taken.
}
\item{subset}{
An index vector specifying the cases to be used in the training
sample.  (NOTE: If given, this argument must be named.)
}
\item{na.action}{
A function to specify the action to be taken if \code{NA}s are found.
The default action is for the procedure to fail.  An alternative is
na.omit, which leads to rejection of cases with missing values on
any required variable.  (NOTE: If given, this argument must be named.)
}
\item{contrasts}{
a list of contrasts to be used for some or all  of
the  factors  appearing as variables in the model formula.
}
\item{Wts}{
initial parameter vector. If missing chosen at random.
}
\item{mask}{
logical vector indicating which parameters should be optimized (default all).
}
\item{linout}{
switch for linear output units. Default logistic output units.
}
\item{entropy}{
switch for entropy (= maximum conditional likelihood) fitting.
Default by least-squares.
}
\item{softmax}{
switch for softmax (log-linear model) and maximum conditional
likelihood fitting. \code{linout}, \code{entropy}, \code{softmax} and \code{censored} are mutually
exclusive.
}
\item{censored}{
A variant on \code{softmax}, in which non-zero targets mean possible
classes. Thus for \code{softmax} a row of \code{(0, 1, 1)} means one example
each of classes 2 and 3, but for \code{censored} it means one example whose
class is only known to be 2 or 3.
}
\item{skip}{
switch to add skip-layer connections from input to output.
}
\item{rang}{
Initial random weights on [-\code{rang}, \code{rang}].  Value about 0.5 unless the
inputs are large, in which case it should be chosen so that
\code{rang} * max(\code{|x|}) is about 1.
}
\item{decay}{
parameter for weight decay.  Default 0.
}
\item{maxit}{
maximum number of iterations. Default 100.
}
\item{Hess}{
If true, the Hessian of the measure of fit at the best set of weights
found is returned as component \code{Hessian}.
}
\item{trace}{
switch for tracing optimization. Default \code{TRUE}.
}
\item{MaxNWts}{
The maximum allowable number of weights.  There is no intrinsic limit
in the code, but increasing \code{MaxNWts} will probably allow fits that
are very slow and time-consuming.
}
\item{abstol}{
Stop if the fit criterion falls below \code{abstol}, indicating an
essentially perfect fit.
}
\item{reltol}{
Stop if the optimizer is unable to reduce the fit criterion by a
factor of at least \code{1 - reltol}.
}
\item{\dots}{
arguments passed to or from other methods.
}}
\value{
object of class \code{"nnet"} or \code{"nnet.formula"}.
Mostly internal structure, but has components

\item{wts}{
the best set of weights found
}
\item{value}{
value of fitting criterion plus weight decay term.
}
\item{fitted.values}{
the fitted values for the training data.
}
\item{residuals}{
  the residuals for the training data.
}
\item{convergence}{
\code{1} if the maximum number of iterations was reached, otherwise \code{0}.
}}
\details{
If the response in \code{formula} is a factor, an appropriate classification
network is constructed; this has one output and entropy fit if the
number of levels is two, and a number of outputs equal to the number
of classes and a softmax output stage for more levels.  If the
response is not a factor, it is passed on unchanged to \code{nnet.default}.

Optimization is done via the BFGS method of \code{\link{optim}}.
}
\references{
  Ripley, B. D. (1996)
  \emph{Pattern Recognition and Neural Networks.} Cambridge.

  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{predict.nnet}}, \code{\link{nnetHess}}
}
\examples{
# use half the iris data
ir <- rbind(iris3[,,1],iris3[,,2],iris3[,,3])
targets <- class.ind( c(rep("s", 50), rep("c", 50), rep("v", 50)) )
samp <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25))
ir1 <- nnet(ir[samp,], targets[samp,], size = 2, rang = 0.1,
            decay = 5e-4, maxit = 200)
test.cl <- function(true, pred) {
    true <- max.col(true)
    cres <- max.col(pred)
    table(true, cres)
}
test.cl(targets[-samp,], predict(ir1, ir[-samp,]))


# or
ird <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]),
        species = factor(c(rep("s",50), rep("c", 50), rep("v", 50))))
ir.nn2 <- nnet(species ~ ., data = ird, subset = samp, size = 2, rang = 0.1,
               decay = 5e-4, maxit = 200)
table(ird$species[-samp], predict(ir.nn2, ird[-samp,], type = "class"))
}
\keyword{neural}
