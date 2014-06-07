\name{plot.diana}
%% almost identical to ./plot.agnes.Rd and quite similar to ./plot.mona.Rd
\alias{plot.diana}
\title{Plots of a Divisive Hierarchical Clustering}
\description{
  Creates plots for visualizing a \code{diana} object.
}
\usage{
\method{plot}{diana}(x, ask = FALSE, which.plots = NULL, main = NULL,
           sub = paste("Divisive Coefficient = ", round(x$dc, digits = 2)),
           adj = 0, nmax.lab = 35, max.strlen = 5, xax.pretty = TRUE, \dots)
}
\arguments{
  \item{x}{an object of class \code{"diana"}, typically created by
    \code{\link{diana}(.)}.}
  \item{ask}{logical; if true and \code{which.plots} is \code{NULL},
    \code{plot.diana} operates in interactive mode, via \code{\link{menu}}.}
  \item{which.plots}{integer vector or NULL (default), the latter
    producing both plots.  Otherwise, \code{which.plots}
    must contain integers of \code{1} for a \emph{banner} plot or \code{2} for a
    dendrogram or ``clustering tree''.}
  \item{main, sub}{main and sub title for the plot, each with a convenient
    default.  See documentation for these arguments in
    \code{\link{plot.default}}.}
  \item{adj}{for label adjustment in \code{\link{bannerplot}()}.}
  \item{nmax.lab}{integer indicating the number of labels which is
    considered too large for single-name labelling the banner plot.}
  \item{max.strlen}{positive integer giving the length to which
    strings are truncated in banner plot labeling.}
  \item{xax.pretty}{logical or integer indicating if
    \code{\link{pretty}(*, n = xax.pretty)} should be used for the x axis.
    \code{xax.pretty = FALSE} is for back compatibility.}
  \item{\dots}{graphical parameters (see \code{\link{par}}) may also
    be supplied and are passed to \code{\link{bannerplot}()} or
    \code{\link{pltree}()}, respectively.}
}
\section{Side Effects}{
  An appropriate plot is produced on the current graphics device. This can
  be one or both of the following choices:
  \cr Banner
  \cr Clustering tree
}
\details{
When \code{ask = TRUE}, rather than producing each plot sequentially,
\code{plot.diana} displays a menu listing all the plots that can be produced.
If the menu is not desired but a pause between plots is still wanted
one must set \code{par(ask= TRUE)} before invoking the plot command.

The banner displays the hierarchy of clusters, and is equivalent to a tree.
See Rousseeuw (1986) or chapter 6 of Kaufman and Rousseeuw (1990).
The banner plots the diameter of each cluster being splitted.
The observations are listed in the order found by the \code{diana}
algorithm, and the numbers in the \code{height} vector are represented
as bars between the observations.

The leaves of the clustering tree are the original observations.
A branch splits up at the diameter of the cluster being splitted.
}
\note{
  In the banner plot,
  observation labels are only printed when the number of observations is
  limited less than \code{nmax.lab} (35, by default), for readability.
  Moreover, observation labels are truncated to maximally
  \code{max.strlen} (5) characters.
}
\references{see those in \code{\link{plot.agnes}}.}
\seealso{
  \code{\link{diana}}, \code{\link{diana.object}},
  \code{\link{twins.object}}, \code{\link{par}}.
}
\examples{
example(diana)# -> dv <- diana(....)

plot(dv, which = 1, nmax.lab = 100)

## wider labels :
op <- par(mar = par("mar") + c(0, 2, 0,0))
plot(dv, which = 1, nmax.lab = 100, max.strlen = 12)
par(op)
}
\keyword{cluster}
\keyword{hplot}

