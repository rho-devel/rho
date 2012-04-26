% file MASS/man/rms.curv.Rd
% copyright (C) 1994-2009 W. N. Venables and B. D. Ripley
%
\name{rms.curv}
\alias{rms.curv}
\alias{print.rms.curv}
\title{
Relative Curvature Measures for Non-Linear Regression
}
\description{
Calculates the root mean square parameter effects and intrinsic relative
curvatures, \eqn{c^\theta}{c^theta} and \eqn{c^\iota}{c^iota}, for a
fitted nonlinear regression, as defined in Bates & Watts, section 7.3,
p. 253ff
}
\usage{
rms.curv(obj)
}
\arguments{
\item{obj}{
Fitted model object of class \code{"nls"}.  The model must be fitted using the
default algorithm.
}
}
\value{
A list of class \code{rms.curv} with components \code{pc} and \code{ic}
for parameter effects and intrinsic relative curvatures multiplied by
sqrt(F), \code{ct} and \code{ci} for \eqn{c^\theta} and \eqn{c^\iota} (unmultiplied),
and \code{C} the C-array as used in section 7.3.1 of Bates & Watts.
}
\details{
The method of section 7.3.1 of Bates & Watts is implemented.  The
function \code{deriv3} should be used generate a model function with first
derivative (gradient) matrix and second derivative (Hessian) array
attributes.  This function should then be used to fit the nonlinear
regression model.

A print method, \code{print.rms.curv}, prints the \code{pc} and
\code{ic} components only, suitably annotated.

If either \code{pc} or \code{ic} exceeds some threshold (0.3 has been
suggested) the curvature is unacceptably high for the planar assumption.
}
\references{
Bates, D. M, and Watts, D. G. (1988)
\emph{Nonlinear Regression Analysis and its Applications.}
Wiley, New York.
}
\seealso{
\code{\link{deriv3}}
}
\examples{
# The treated sample from the Puromycin data
mmcurve <- deriv3(~ Vm * conc/(K + conc), c("Vm", "K"),
                  function(Vm, K, conc) NULL)
Treated <- Puromycin[Puromycin$state == "treated", ]
(Purfit1 <- nls(rate ~ mmcurve(Vm, K, conc), data = Treated,
                start = list(Vm=200, K=0.1)))
rms.curv(Purfit1)
##Parameter effects: c^theta x sqrt(F) = 0.2121
##        Intrinsic: c^iota  x sqrt(F) = 0.092
}
\keyword{nonlinear}
