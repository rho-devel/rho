\name{cv.glm}
\alias{cv.glm}
\title{
Cross-validation for Generalized Linear Models
}
\description{
This function calculates the estimated K-fold cross-validation prediction 
error for generalized linear models.
}
\usage{
cv.glm(data, glmfit, cost, K)
}
\arguments{
\item{data}{
A matrix or data frame containing the data.  The rows should be cases and
the columns correspond to variables, one of which is the response.
}
\item{glmfit}{
An object of class \code{"glm"} containing the results of a generalized linear
model fitted to \code{data}.
}
\item{cost}{
A function of two vector arguments specifying the cost function for the 
cross-validation.  The first argument to \code{cost} should correspond to the
observed responses and the second argument should correspond to the predicted
or fitted responses from the generalized linear model.  \code{cost} must return a
non-negative scalar value.  The default is the average squared error function.
}
\item{K}{
The number of groups into which the data should be split to estimate the
cross-validation prediction error.  The value of \code{K} must be such that all
groups are of approximately equal size.  If the supplied value of \code{K} does
not satisfy this criterion then it will be set to the closest integer which
does and a warning is generated specifying the value of \code{K} used.  The default
is to set \code{K} equal to the number of observations in \code{data} which gives the
usual leave-one-out cross-validation.
}}
\value{
The returned value is a list with the following components.

\item{call}{
The original call to \code{cv.glm}.
}
\item{K}{
The value of \code{K} used for the K-fold cross validation.
}
\item{delta}{
A vector of length two.  The first component is the raw cross-validation
estimate of prediction error.  The second component is the adjusted
cross-validation estimate.  The adjustment is designed to compensate for the
bias introduced by not using leave-one-out cross-validation.
}
\item{seed}{
The value of \code{.Random.seed} when \code{cv.glm} was called. 
}}
\section{Side Effects}{
The value of \code{.Random.seed} is updated.
}
\details{
The data is divided randomly into \code{K} groups.  For each group the generalized
linear model is fit to \code{data} omitting that group, then the function \code{cost}
is applied to the observed responses in the group that was omitted from the fit
and the prediction made by the fitted models for those observations.


When \code{K} is the number of observations leave-one-out cross-validation is used
and all the possible splits of the data are used.  When \code{K} is less than
the number of observations the \code{K} splits to be used are found by randomly
partitioning the data into \code{K} groups of approximately equal size.  In this
latter case a certain amount of bias is introduced.  This can be reduced by
using a simple adjustment (see equation 6.48 in Davison and Hinkley, 1997).
The second value returned in \code{delta} is the estimate adjusted by this method.


}
\references{
Breiman, L., Friedman, J.H., Olshen, R.A. and Stone, C.J. (1984)
\emph{Classification and Regression Trees}. Wadsworth.


Burman, P. (1989) A comparative study of ordinary cross-validation, 
\emph{v}-fold cross-validation and repeated learning-testing methods.
\emph{Biometrika}, \bold{76}, 503--514


Davison, A.C. and Hinkley, D.V. (1997) 
\emph{Bootstrap Methods and Their Application}. Cambridge University Press.


Efron, B. (1986) How biased is the apparent error rate of a prediction rule?
\emph{Journal of the American Statistical Association}, \bold{81}, 461--470.


Stone, M.  (1974) Cross-validation choice and assessment of statistical
predictions (with Discussion). 
\emph{Journal of the Royal Statistical Society, B}, \bold{36}, 111--147.
}
\seealso{
\code{\link{glm}}, \code{\link{glm.diag}}, \code{\link{predict}}
}
\examples{
# leave-one-out and 6-fold cross-validation prediction error for 
# the mammals data set.
data(mammals, package="MASS")
mammals.glm <- glm(log(brain) ~ log(body), data = mammals)
(cv.err <- cv.glm(mammals, mammals.glm)$delta)
(cv.err.6 <- cv.glm(mammals, mammals.glm, K = 6)$delta)

# As this is a linear model we could calculate the leave-one-out 
# cross-validation estimate without any extra model-fitting.
muhat <- fitted(mammals.glm)
mammals.diag <- glm.diag(mammals.glm)
(cv.err <- mean((mammals.glm$y - muhat)^2/(1 - mammals.diag$h)^2))


# leave-one-out and 11-fold cross-validation prediction error for 
# the nodal data set.  Since the response is a binary variable an
# appropriate cost function is
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

nodal.glm <- glm(r ~ stage+xray+acid, binomial, data = nodal)
(cv.err <- cv.glm(nodal, nodal.glm, cost, K = nrow(nodal))$delta)
(cv.11.err <- cv.glm(nodal, nodal.glm, cost, K = 11)$delta)
}
\keyword{regression}
