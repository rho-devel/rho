\name{full.score}
\alias{full.score}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{GCV/UBRE score for use within nlm}
\description{ Evaluates GCV/UBRE score for a GAM, given smoothing
parameters. The routine calls \code{\link{gam.fit}} to fit the model, and is
usually called by \code{\link{nlm}} to optimize the smoothing parameters. 

This is basically a service routine for \code{\link{gam}}, and is not usually 
called directly by users. It is only used in this context for GAMs fitted by
outer iteration (see \code{\link{gam.outer}}) when the the outer method is 
\code{"nlm.fd"} (see \code{\link{gam}} argument \code{optimizer}).
}
\usage{
full.score(sp,G,family,control,gamma,...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
\item{sp}{The logs of the smoothing parameters}

\item{G}{a list returned by \code{mgcv:::gam.setup}}

\item{family}{The family object for the GAM.}

\item{control}{a list returned be \code{\link{gam.control}}}

\item{gamma}{the degrees of freedom inflation factor (usually 1).}


\item{...}{other arguments, typically for passing on to \code{gam.fit}.}
}
\value{ The value of the GCV/UBRE score, with attribute \code{"full.gam.object"}
which is the full object returned by \code{\link{gam.fit}}.

}

\author{ Simon N. Wood \email{simon.wood@r-project.org}}


\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ..









