
\name{spasm.construct}
\alias{spasm.construct}
\alias{spasm.sp}
\alias{spasm.smooth}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Experimental sparse smoothers}
\description{These are experimental sparse smoothing functions, and should 
be left well alone!
}

\usage{
spasm.construct(object,data)
spasm.sp(object,sp,w=rep(1,object$nobs),get.trH=TRUE,block=0,centre=FALSE)
spasm.smooth(object,X,residual=FALSE,block=0)
}
%- maybe also `usage' for other objects documented here.
\arguments{
\item{object}{sparse smooth object}
\item{data}{data frame}
\item{sp}{smoothing parameter value}
\item{w}{optional weights}
\item{get.trH}{Should (estimated) trace of sparse smoother matrix be returned}
\item{block}{index of block, 0 for all blocks}
\item{centre}{should sparse smooth be centred?}
\item{X}{what to smooth}
\item{residual}{apply residual operation?}
}

%\value{}

%\details{} 


%\references{}



\author{Simon N. Wood \email{simon.wood@r-project.org}}

%\seealso{} 

\section{WARNING}{It is not recommended to use these yet}

%\examples{}

\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ...

