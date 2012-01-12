\name{gam.check}
\alias{gam.check}
\title{Some diagnostics for a fitted gam model}
\description{ Takes a fitted \code{gam} object produced by \code{gam()} and produces some diagnostic information
about the fitting procedure and results. The default is to produce 4 residual
plots, and some information about the convergence of the smoothness selection optimization.
}
\usage{
gam.check(b, old.style=FALSE,
          type=c("deviance","pearson","response"),
          rep=0, level=.9, rl.col=2, rep.col="gray80", \dots)
}
\arguments{
  \item{b}{a fitted \code{gam} object as produced by \code{\link{gam}()}.}
  \item{old.style}{If you want old fashioned plots, exactly as in Wood, 2006, set to \code{TRUE}.}
  \item{type}{type of residuals, see \code{\link{residuals.gam}}, used in
    all plots.}
  \item{rep, level, rl.col, rep.col}{
    arguments passed to \code{\link{qq.gam}()} when \code{old.style} is
    false, see there.}
  \item{\dots}{extra graphics parameters to pass to plotting functions.}
}

\value{A vector of reference quantiles for the residual distribution, if these can be computed.}

\details{ This function plots 4 standard diagnostic plots, and some other
convergence diagnostics. Usually the 4 plots are various residual plots. The
printed information relates to the optimization used to select smoothing
parameters. For the default optimization methods the information is summarized in a
readable way, but for other optimization methods, whatever is returned by way of
convergence diagnostics is simply printed.

The QQ plot produced is usually created by a call to \code{\link{qq.gam}}, and plots deviance residuals 
against approximate theoretical quantilies of the deviance residual distribution, according to the fitted model. 
If this looks odd then investigate further using \code{\link{qq.gam}}. Note that residuals for models fitted to binary data contain very little 
information useful for model checking (it is necessary to find some way of aggregating them first), so the QQ plot is unlikely 
to be useful in this case.

}

\references{

Wood S.N. (2006) Generalized Additive Models: An Introduction with R. Chapman
and Hall/CRC Press.


\url{http://www.maths.bath.ac.uk/~sw283/}

}
\author{ Simon N. Wood \email{simon.wood@r-project.org}}


\seealso{ \code{\link{choose.k}},  \code{\link{gam}}, \code{\link{mgcv}}, \code{\link{magic}}}

\examples{
library(mgcv)
set.seed(0)
dat <- gamSim(1,n=200)
b<-gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)
plot(b,pages=1)
gam.check(b,pch=19,cex=.3)
}
\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ...






