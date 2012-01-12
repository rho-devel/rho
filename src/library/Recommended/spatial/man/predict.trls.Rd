\name{predict.trls}
\alias{predict.trls}
\title{
Predict method for trend surface fits
}
\description{
Predicted values based on trend surface model object
}
\usage{
\method{predict}{trls}(object, x, y, \dots)
}
\arguments{
\item{object}{
Fitted trend surface model object returned by \code{surf.ls}
}
\item{x}{
Vector of prediction location eastings (x coordinates)
}
\item{y}{
Vector of prediction location northings (y coordinates)
}
\item{\dots}{
further arguments passed to or from other methods.
}}
\value{
\code{predict.trls} produces a vector of predictions corresponding
to the prediction locations. To display the output with \code{image}
or \code{contour}, use \code{trmat} or convert the returned vector
to matrix form.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\seealso{
\code{\link{surf.ls}}, \code{\link{trmat}}
}
\examples{
data(topo, package="MASS")
topo2 <- surf.ls(2, topo)
topo4 <- surf.ls(4, topo)
x <- c(1.78, 2.21)
y <- c(6.15, 6.15)
z2 <- predict(topo2, x, y)
z4 <- predict(topo4, x, y)
cat("2nd order predictions:", z2, "\\n4th order predictions:", z4, "\\n")
}
\keyword{spatial}
