\name{glm.diag.plots}
\alias{glm.diag.plots}
\title{
Diagnostics plots for generalized linear models
}
\description{
Makes plot of jackknife deviance residuals against linear predictor, 
normal scores plots of standardized deviance residuals, plot of approximate Cook statistics against leverage/(1-leverage), and case plot of Cook statistic.
}
\usage{
glm.diag.plots(glmfit, glmdiag = glm.diag(glmfit), subset = NULL,
               iden = FALSE, labels = NULL, ret = FALSE)
}
\arguments{
\item{glmfit}{
\code{glm.object} : the result of a call to \code{glm()}
}
\item{glmdiag}{
Diagnostics of \code{glmfit} obtained from a call to \code{glm.diag}.  If
it is not supplied then it is calculated.  
}
\item{subset}{
Subset of \code{data} for which \code{glm} fitting performed: should be the same as the 
\code{subset} option used in the call to \code{glm()} which generated \code{glmfit}.  Needed 
only if the \code{subset=} option was used in the call to \code{glm}.  
}
\item{iden}{
A logical argument. If \code{TRUE} then, after the plots are drawn, the user will
be prompted for an integer between 0 and 4.  A positive integer will select
a plot and invoke \code{identify()} on that plot.  After exiting \code{identify()}, the
user is again prompted, this loop continuing until the user responds to the
prompt with 0.  If \code{iden} is \code{FALSE} (default) the user cannot interact with the plots.
}
\item{labels}{
A vector of labels for use with \code{identify()} if \code{iden} is \code{TRUE}.  If it is not 
supplied then the labels are derived from \code{glmfit}.
}
\item{ret}{
A logical argument indicating if \code{glmdiag} should be returned.  The default is
\code{FALSE}.
}}
\value{
If \code{ret} is \code{TRUE} then the value of \code{glmdiag} is returned otherwise there is
no returned value.
}
\details{
The diagnostics required for the plots are calculated by \code{glm.diag}.  These are
then used to produce the four plots on the current graphics device.


The plot on the top left is a plot of the jackknife deviance residuals 
against the fitted values.


The plot on the top right is a normal QQ plot of the standardized deviance 
residuals.  The dotted line is the expected line if the standardized residuals
are normally distributed, i.e. it is the line with intercept 0 and slope 1.


The bottom two panels are plots of the Cook statistics.  On the left is a plot
of the Cook statistics against the standardized leverages.  In general there
will be two dotted lines on this plot.  The horizontal line is at 8/(n-2p)
where n is the number of observations and p is the number of parameters 
estimated.  Points above this line may be points with high influence on the
model.  The vertical line is at 2p/(n-2p) and points to the right of this
line have high leverage compared to the variance of the raw residual at that 
point.  If all points are below the horizontal line or to the left of the
vertical line then the line is not shown.


The final plot again shows the Cook statistic this time plotted against case
number enabling us to find which observations are influential.


Use of \code{iden=T} is encouraged for proper exploration of these four plots as
a guide to how well the model fits the data and whether certain observations
have an unduly large effect on parameter estimates.
}
\section{Side Effects}{
The current device is cleared and four plots are plotted by use of
\code{split.screen(c(2,2))}.  If \code{iden} is \code{TRUE}, interactive identification of 
points is enabled.  All screens are closed, but not cleared, on termination of 
the function.
}
\references{
Davison, A. C. and Hinkley, D. V. (1997) 
\emph{Bootstrap Methods and Their Application}. Cambridge University Press.


Davison, A.C. and Snell, E.J.  (1991)  Residuals and diagnostics.  In 
\emph{Statistical Theory and Modelling: In Honour of Sir David Cox}
D.V. Hinkley, N. Reid, and E.J. Snell (editors), 83--106. Chapman and Hall.
}
\seealso{
\code{\link{glm}}, \code{\link{glm.diag}}, \code{\link{identify}}
}
\examples{
# In this example we look at the leukaemia data which was looked at in 
# Example 7.1 of Davison and Hinkley (1997)
data(leuk, package = "MASS")
leuk.mod <- glm(time ~ ag-1+log10(wbc), family = Gamma(log), data = leuk)
leuk.diag <- glm.diag(leuk.mod)
glm.diag.plots(leuk.mod, leuk.diag)
}
\keyword{regression}
\keyword{dplot}
\keyword{hplot}
