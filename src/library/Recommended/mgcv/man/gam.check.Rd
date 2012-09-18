\name{gam.check}
\alias{gam.check}
\title{Some diagnostics for a fitted gam model}
\description{ Takes a fitted \code{gam} object produced by \code{gam()} and produces some diagnostic information
about the fitting procedure and results. The default is to produce 4 residual
plots, some information about the convergence of the smoothness selection optimization, and to run 
diagnostic tests of whether the basis dimension choises are adequate. 
}
\usage{
gam.check(b, old.style=FALSE,
          type=c("deviance","pearson","response"),
          k.sample=5000,k.rep=200,
          rep=0, level=.9, rl.col=2, rep.col="gray80", \dots)
}
\arguments{
  \item{b}{a fitted \code{gam} object as produced by \code{\link{gam}()}.}
  \item{old.style}{If you want old fashioned plots, exactly as in Wood, 2006, set to \code{TRUE}.}
  \item{type}{type of residuals, see \code{\link{residuals.gam}}, used in
    all plots.}
  \item{k.sample}{Above this k testing uses a random sub-sample of data.}
  \item{k.rep}{how many re-shuffles to do to get p-value for k testing.}
  \item{rep, level, rl.col, rep.col}{
    arguments passed to \code{\link{qq.gam}()} when \code{old.style} is
    false, see there.}
  \item{\dots}{extra graphics parameters to pass to plotting functions.}
}

\value{A vector of reference quantiles for the residual distribution, if these can be computed.}

\details{ Checking a fitted \code{gam} is like checking a fitted \code{glm}, with two main differences. Firstly, 
the basis dimensions used for smooth terms need to be checked, to ensure that they are not so small that they force 
oversmoothing: the defaults are arbitrary. \code{\link{choose.k}} provides more detail, but the diagnostic tests described below and reported by this function may also help. Secondly, fitting may not always be as robust to violation of the distributional assumptions as would be the case for a regular GLM, so slightly more care may be needed here. In particular, the thoery of quasi-likelihood implies that if the mean variance relationship is OK for a GLM, then other departures from the assumed distribution are not problematic: GAMs can sometimes be more sensitive. For example, un-modelled overdispersion will typically lead to overfit, as the smoothness selection criterion tries to reduce the scale parameter to the one specified. Similarly, it is not clear how sensitive REML and ML smoothness selection will be to deviations from the assumed response dsistribution. For these reasons this routine uses an enhanced residual QQ plot.


This function plots 4 standard diagnostic plots,  some smoothing parameter estimation
convergence information and the results of tests which may indicate if the smoothing basis dimension
for a term is too low.  

Usually the 4 plots are various residual plots. For the default optimization methods the convergence information is summarized in a readable way, but for other optimization methods, whatever is returned by way of
convergence diagnostics is simply printed. 

The test of whether the basis dimension for a smooth is adequate is based on computing an estimate of the residual variance 
based on differencing residuals that are near neighbours according to the (numeric) covariates of the smooth. This estimate divided by the residual variance is the \code{k-index} reported. The further below 1 this is, the more likely it is that there is missed pattern left in the residuals. The \code{p-value} is computed by simulation: the residuals are randomly re-shuffled \code{k.rep} times to obtain the null distribution of the differencing variance estimator, if there is no pattern in the residuals. For models fitted to more than \code{k.sample} data, the tests are based of \code{k.sample} randomly sampled data. Low p-values may indicate that the basis dimension, \code{k}, has been set too low, especially if the reported \code{edf} is close to \code{k\'}, the maximum possible EDF for the term. Note the disconcerting fact that if the test statistic itself is based on random resampling and the null is true, then the associated p-values will of course vary widely from one replicate to the next. Currently smooths of factor variables are not supported and will give an \code{NA} p-value.

 Doubling a suspect \code{k} and re-fitting is sensible: if the reported \code{edf} increases substantially then you may have been missing something in the first fit. Of course p-values can be low for  reasons other than a too low \code{k}. See \code{\link{choose.k}} for fuller discussion.

The QQ plot produced is usually created by a call to \code{\link{qq.gam}}, and plots deviance residuals 
against approximate theoretical quantilies of the deviance residual distribution, according to the fitted model. 
If this looks odd then investigate further using \code{\link{qq.gam}}. Note that residuals for models fitted to binary data contain very little 
information useful for model checking (it is necessary to find some way of aggregating them first), so the QQ plot is unlikely 
to be useful in this case.

}

\references{

N.H. Augustin, E-A Sauleaub, S.N. Wood (2012) On quantile quantile plots for generalized linear models.
Computational Statistics & Data Analysis. 56(8), 2404-3409.

Wood S.N. (2006) Generalized Additive Models: An Introduction with R. Chapman
and Hall/CRC Press.


\url{http://www.maths.bath.ac.uk/~sw283/}

}
\author{ Simon N. Wood \email{simon.wood@r-project.org}}


\seealso{ \code{\link{choose.k}},  \code{\link{gam}}, \code{\link{magic}}}

\examples{
library(mgcv)
set.seed(0)
dat <- gamSim(1,n=200)
b<-gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)
plot(b,pages=1)
gam.check(b,pch=19,cex=.3)
}
\keyword{models} \keyword{smooth} \keyword{regression}%-- one or more ...






