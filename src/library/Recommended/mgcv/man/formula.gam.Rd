\name{formula.gam}
\alias{formula.gam}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{GAM formula}
\description{ Description of \code{\link{gam}} formula (see Details), and how to extract it from a fitted \code{gam} object.
}
\usage{
\method{formula}{gam}(x,...)
}
%- maybe also `usage' for other objects documented here.
\arguments{ 
\item{x}{ fitted model objects of class \code{gam} (see \code{\link{gamObject}}) as produced by \code{gam()}.}
\item{...}{un-used in this case} 
}

\value{ Returns the model formula, \code{x$formula}. Provided so that \code{anova} methods
print an appropriate description of the model.
}

\details{ The formula supplied to \code{\link{gam}} is exactly like that supplied to 
\code{\link{glm}} except that smooth terms, \code{\link{s}} and \code{\link{te}} can
be added to the right hand side (and \code{.} is not supported in \code{gam} formulae).

Smooth terms are specified by expressions of the form: \cr
\code{s(x1,x2,...,k=12,fx=FALSE,bs="tp",by=z,id=1)}\cr
 where \code{x1}, \code{x2}, etc. are the covariates which the smooth
is a function of, and \code{k} is the dimension of the basis used to
represent the smooth term. If \code{k} is not
 specified then basis specific defaults are used. Note that these defaults are
essentially arbitrary, and it  is important to check that they are not so 
small that they cause oversmoothing (too large just slows down computation). 
Sometimes the modelling context suggests sensible values for \code{k}, but if not
informal checking is easy: see \code{\link{choose.k}} and \code{\link{gam.check}}.  


\code{fx} is used to indicate whether or not this term should be unpenalized, 
and therefore have a fixed number of degrees of freedom set by \code{k} 
(almost always \code{k-1}). \code{bs} indicates the basis to use for the smooth:
the built in options are described in \code{\link{smooth.terms}}, and user defined 
smooths can be added (see \code{\link{user.defined.smooth}}). If \code{bs} is not supplied 
then the default \code{"tp"} (\code{\link{tprs}}) basis is used. 
\code{by} can be used to specify a variable by which
the smooth should be multiplied. For example \code{gam(y~s(x,by=z))}
would specify a model \eqn{ E(y) = f(x)z}{E(y)=f(x)z} where
\eqn{f(\cdot)}{f(.)} is a smooth function. The \code{by} 
option is particularly useful for models in
which different functions of the same variable are required for
each level of a factor and for `varying coefficient models': see \code{\link{gam.models}}. 
\code{id} is used to give smooths identities: smooths with the same identity have
the same basis, penalty and smoothing parameter (but different coefficients, so they are 
different functions). 

An alternative for specifying smooths of more than one covariate is e.g.: \cr
\code{te(x,z,bs=c("tp","tp"),m=c(2,3),k=c(5,10))}\cr which would specify a tensor product 
smooth of the two covariates \code{x} and \code{z} constructed from marginal t.p.r.s. bases 
of dimension 5 and 10 with marginal penalties of order 2 and 3. Any combination of basis types is 
possible, as is any number of covariates. \code{\link{te}} provides further information. 
\code{\link{ti}} terms are a variant designed to be used as interaction terms when the main 
effects (and any lower order interactions) are present. \code{\link{t2}} produces tensor product
smooths that are the natural low rank analogue of smoothing spline anova models. 

\code{s}, \code{te}, \code{ti} and \code{t2} terms accept an \code{sp} argument of supplied smoothing parameters: positive 
values are taken  as fixed values to be used, negative to indicate that the parameter should be estimated. If
\code{sp} is supplied then it over-rides whatever is in the \code{sp} argument to \code{gam}, if it is not supplied 
then it defaults to all negative, but does not over-ride the \code{sp} argument to \code{gam}.

Formulae can involve nested or ``overlapping'' terms such as \cr
\code{y~s(x)+s(z)+s(x,z)} or \code{y~s(x,z)+s(z,v)}\cr 
but nested models should really be set up using \code{\link{ti}} terms:
see \code{\link{gam.side}} for further details and examples. 

Smooth terms in a \code{gam} formula will accept matrix arguments as covariates (and corresponding \code{by} variable), 
in which case a `summation convention' is invoked. Consider the example of \code{s(X,Z,by=L)} where \code{X}, \code{Z}
and \code{L} are n by m matrices. Let \code{F} be the n by m matrix that results from evaluating the smooth at the values in 
\code{X} and \code{Z}. Then the contribution to the linear predictor from the term will be
\code{rowSums(F*L)} (note the element-wise multiplication). This convention allows the linear predictor of the GAM
to depend on (a discrete approximation to) any linear functional of a smooth: see \code{\link{linear.functional.terms}} for more information and examples (including functional linear models/signal regression).

Note that \code{gam} allows any term in the model formula to be penalized (possibly by multiple penalties), 
via the \code{paraPen} argument. See 
\code{\link{gam.models}} for details and example code.

}



\author{ Simon N. Wood \email{simon.wood@r-project.org}}


\seealso{  \code{\link{gam}}}


\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ...






