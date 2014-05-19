\name{gam.convergence}
\alias{gam.convergence}
\alias{gam.performance}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{GAM convergence and performance issues}
\description{ When fitting GAMs there is a tradeoff between speed of 
fitting and probability of fit convergence. The default fitting options, 
specified by
\code{\link{gam}} arguments \code{method} and \code{optimizer}, 
opt for certainty of convergence over speed of
fit. In the Generalized Additive Model case it means using
`outer' iteration in preference to `performance iteration': see
\code{\link{gam.outer}} for details. 

It is possible for the default `outer' iteration to fail when finding intial
smoothing parameters using a few steps of performance iteration (if you get a
convergence failure message from \code{magic} when outer iterating, then this
is what has happened): lower \code{outerPIsteps} in \code{\link{gam.control}}
to fix this. 


There are three things that you can try to speed up GAM fitting. (i) if you have large 
numbers of smoothing parameters in the generalized case, then try the \code{"bfgs"} method 
option in \code{\link{gam}} argument \code{optimizer}: this can be faster than the default. (ii) Change the
\code{optimizer} argument to \code{\link{gam}} so that `performance iteration' is
used in place of the default outer iteration. Usually performance iteration
converges well and it can sometimes be quicker than the default outer iteration. 
(iii) For large datasets it may be worth changing
the smoothing basis to use \code{bs="cr"} (see \code{\link{s}} for details)
for 1-d smooths, and to use \code{\link{te}} smooths in place of
\code{\link{s}} smooths for smooths of more than one variable. This is because
the default thin plate regression spline basis \code{"tp"} is costly to set up
for large datasets (much over 1000 data, say). (iv) consider using \code{\link{bam}}.

If the GAM estimation process fails to converge when using performance
iteration, then switch to outer iteration via the \code{optimizer} argument of
\code{\link{gam}}. If it still fails, try
increasing the number of IRLS iterations (see \code{\link{gam.control}}) or
perhaps experiment with the convergence tolerance. 

If you still have problems, it's worth noting that a GAM is just a (penalized)
GLM and the IRLS scheme used to estimate GLMs is not guaranteed to
converge. Hence non convergence of a GAM may relate to a lack of stability in
the basic IRLS scheme. Therefore it is worth trying to establish whether the IRLS iterations
are capable of converging. To do this fit the problematic GAM with all smooth
terms specified with \code{fx=TRUE} so that the smoothing parameters are all
fixed at zero. If this `largest' model can converge then, then the maintainer 
would quite like to know about your problem! If it doesn't converge, then its
likely that your model is just too flexible for the IRLS process itself. Having tried
increasing \code{maxit} in \code{gam.control}, there are several other
possibilities for stabilizing the iteration. It is possible to try (i) setting lower bounds on the
smoothing parameters using the \code{min.sp} argument of \code{gam}: this may
or may not change the model being fitted; (ii)
reducing the flexibility of the model by reducing the basis dimensions
\code{k} in the specification of \code{s} and \code{te} model terms: this
obviously changes the model being fitted somewhat; (iii)
introduce a small regularization term into the fitting via the \code{irls.reg}
argument of \code{gam.control}: this option obviously changes the nature of
the fit somewhat, since parameter estimates are pulled towards zero by doing
this. 

Usually, a major contributer to fitting difficulties is that the
model is a very poor description of the data.  
 

}

%- maybe also `usage' for other objects documented here.


\author{ Simon N. Wood \email{simon.wood@r-project.org}}

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..
