% file MASS/man/glm.nb.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{glm.nb}
\alias{glm.nb}
\alias{family.negbin}
\alias{logLik.negbin}
\title{
Fit a Negative Binomial Generalized Linear Model
}
\description{
  A modification of the system function \code{\link{glm}()} to include
  estimation of the additional parameter, \code{theta}, for a
  Negative Binomial generalized linear model.
}
%\usage{
%glm.nb(formula, \dots,  init.theta, link = log)
%}
\usage{
glm.nb(formula, data, weights, subset, na.action,
       start = NULL, etastart, mustart,
       control = glm.control(...), method = "glm.fit",
       model = TRUE, x = FALSE, y = TRUE, contrasts = NULL, ...,
       init.theta, link = log)
}
\arguments{
\item{formula, data, weights, subset, na.action, start, etastart,
  mustart, control, method, model, x, y, contrasts, \dots}{
  arguments for the \code{\link{glm}()} function.
  Note that these exclude \code{family} and \code{offset}
  (but \code{\link{offset}()} can be used).
}
\item{init.theta}{
  Optional initial value for the theta parameter.  If omitted a moment
  estimator after an initial fit using a Poisson GLM is used.
}
\item{link}{
  The link function.  Currently must be one of \code{log}, \code{sqrt}
  or \code{identity}.
}}
\value{
  A fitted model object of class \code{negbin} inheriting from \code{glm}
  and \code{lm}.  The object is like the output of \code{glm} but contains
  three additional components, namely \code{theta} for the ML estimate of
  theta, \code{SE.theta} for its approximate standard error (using
  observed rather than expected information), and \code{twologlik} for
  twice the log-likelihood function.
}
\details{
  An alternating iteration process is used.  For given \code{theta} the GLM
  is fitted using the same process as used by \code{glm()}.  For fixed means
  the \code{theta} parameter is estimated using score and information
  iterations.  The two are alternated until convergence of both. (The
  number of alternations and the number of iterations when estimating
  \code{theta} are controlled by the \code{maxit} parameter of
  \code{glm.control}.)

  Setting \code{trace > 0} traces the alternating iteration
  process. Setting \code{trace > 1} traces the \code{glm} fit, and
  setting \code{trace > 2} traces the estimation of \code{theta}.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
  \code{\link{glm}}, \code{\link{negative.binomial}},
  \code{\link{anova.negbin}}, \code{\link{summary.negbin}},
  \code{\link{theta.md}}

  There is a \code{\link{simulate}} method.
}
\examples{
quine.nb1 <- glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = quine)
quine.nb2 <- update(quine.nb1, . ~ . + Sex:Age:Lrn)
quine.nb3 <- update(quine.nb2, Days ~ .^4)
anova(quine.nb1, quine.nb2, quine.nb3)
\dontshow{## PR#1695
y <- c(7, 5, 4, 7, 5, 2, 11, 5, 5, 4, 2, 3, 4, 3, 5, 9, 6, 7, 10, 6, 12,
6, 3, 5, 3, 9, 13, 0, 6, 1, 2, 0, 1, 0, 0, 4, 5, 1, 5, 3, 3, 4)

lag1 <- c(0, 7, 5, 4, 7, 5, 2, 11, 5, 5, 4, 2, 3, 4, 3, 5, 9, 6, 7, 10,
6, 12, 6, 3, 5, 3, 9, 13, 0, 6, 1, 2, 0, 1, 0, 0, 4, 5, 1, 5, 3, 3)

lag2 <- c(0, 0, 7, 5, 4, 7, 5, 2, 11, 5, 5, 4, 2, 3, 4, 3, 5, 9, 6, 7,
10, 6, 12, 6, 3, 5, 3, 9, 13, 0, 6, 1, 2, 0, 1, 0, 0, 4, 5, 1, 5, 3)

lag3 <- c(0, 0, 0, 7, 5, 4, 7, 5, 2, 11, 5, 5, 4, 2, 3, 4, 3, 5, 9, 6,
7, 10, 6, 12, 6, 3, 5, 3, 9, 13, 0, 6, 1, 2, 0, 1, 0, 0, 4, 5, 1, 5)

(fit <- glm(y ~ lag1+lag2+lag3, family=poisson(link=identity),
            start=c(2, 0.1, 0.1, 0.1)))
try(glm.nb(y ~ lag1+lag2+lag3, link=identity))
glm.nb(y ~ lag1+lag2+lag3, link=identity,  start=c(2, 0.1, 0.1, 0.1))
glm.nb(y ~ lag1+lag2+lag3, link=identity,  start=coef(fit))
glm.nb(y ~ lag1+lag2+lag3, link=identity, etastart=rep(5, 42))
}}
\keyword{regression}
\keyword{models}
