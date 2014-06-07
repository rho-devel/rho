% file MASS/man/kde2d.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{kde2d}
\alias{kde2d}
\title{
Two-Dimensional Kernel Density Estimation
}
\description{
Two-dimensional kernel density estimation with an axis-aligned
bivariate normal kernel, evaluated on a square grid.
}
\usage{
kde2d(x, y, h, n = 25, lims = c(range(x), range(y)))
}
\arguments{
\item{x}{
x coordinate of data
}
\item{y}{
y coordinate of data
}
\item{h}{
vector of bandwidths for x and y directions.  Defaults to
normal reference bandwidth (see \code{\link{bandwidth.nrd}}). A scalar
value will be taken to apply to both directions.
}
\item{n}{
Number of grid points in each direction.  Can be scalar or a length-2
integer vector.
}
\item{lims}{
The limits of the rectangle covered by the grid as \code{c(xl, xu, yl, yu)}.
}}
\value{
A list of three components.

\item{x, y}{
The x and y coordinates of the grid points, vectors of length \code{n}.
}
\item{z}{
An \code{n[1]} by \code{n[2]} matrix of the estimated density: rows
correspond to the value of \code{x}, columns to the value of \code{y}. 
}}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S.} Fourth edition.  Springer.
}
\examples{
attach(geyser)
plot(duration, waiting, xlim = c(0.5,6), ylim = c(40,100))
f1 <- kde2d(duration, waiting, n = 50, lims = c(0.5, 6, 40, 100))
image(f1, zlim = c(0, 0.05))
f2 <- kde2d(duration, waiting, n = 50, lims = c(0.5, 6, 40, 100),
            h = c(width.SJ(duration), width.SJ(waiting)) )
image(f2, zlim = c(0, 0.05))
persp(f2, phi = 30, theta = 20, d = 5)

plot(duration[-272], duration[-1], xlim = c(0.5, 6),
     ylim = c(1, 6),xlab = "previous duration", ylab = "duration")
f1 <- kde2d(duration[-272], duration[-1],
            h = rep(1.5, 2), n = 50, lims = c(0.5, 6, 0.5, 6))
contour(f1, xlab = "previous duration",
        ylab = "duration", levels  =  c(0.05, 0.1, 0.2, 0.4) )
f1 <- kde2d(duration[-272], duration[-1],
            h = rep(0.6, 2), n = 50, lims = c(0.5, 6, 0.5, 6))
contour(f1, xlab = "previous duration",
        ylab = "duration", levels  =  c(0.05, 0.1, 0.2, 0.4) )
f1 <- kde2d(duration[-272], duration[-1],
            h = rep(0.4, 2), n = 50, lims = c(0.5, 6, 0.5, 6))
contour(f1, xlab = "previous duration",
        ylab = "duration", levels  =  c(0.05, 0.1, 0.2, 0.4) )
detach("geyser")
}
\keyword{dplot}
