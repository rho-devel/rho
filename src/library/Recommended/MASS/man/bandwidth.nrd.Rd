\name{bandwidth.nrd}
\alias{bandwidth.nrd}
\title{
Bandwidth for density() via Normal Reference Distribution
}
\description{
A well-supported rule-of-thumb for choosing the bandwidth of a Gaussian
kernel density estimator.
}
\usage{
bandwidth.nrd(x)
}
\arguments{
\item{x}{
A data vector.
}}
\value{
  A bandwidth on a scale suitable for the \code{width} argument of
  \code{density}.
}
\references{
Venables, W. N. and Ripley, B. D. (2002)
\emph{Modern Applied Statistics with S.}
Springer, equation (5.5) on page 130.
}
\examples{
# The function is currently defined as
function(x)
{
    r <- quantile(x, c(0.25, 0.75))
    h <- (r[2] - r[1])/1.34
    4 * 1.06 * min(sqrt(var(x)), h) * length(x)^(-1/5)
}
}
\keyword{dplot}
