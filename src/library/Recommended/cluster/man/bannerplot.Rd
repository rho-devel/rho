\name{bannerplot}
\alias{bannerplot}
\title{Plot Banner (of Hierarchical Clustering)}
\description{
  Draws a \dQuote{banner}, i.e. basically a horizontal \code{\link{barplot}}
  visualizing the (agglomerative or divisive) hierarchical clustering or
  an other binary dendrogram structure.
}
\usage{
bannerplot(x, w = rev(x$height), fromLeft = TRUE,
           main=NULL, sub=NULL, xlab = "Height",  adj = 0,
           col = c(2, 0), border = 0, axes = TRUE, frame.plot = axes,
           rev.xax = !fromLeft, xax.pretty = TRUE,
           labels = NULL, nmax.lab = 35, max.strlen = 5,
           yax.do = axes && length(x$order) <= nmax.lab,
           yaxRight = fromLeft, y.mar = 2.4 + max.strlen/2.5, \dots)
}
\arguments{
  \item{x}{a list with components \code{order}, \code{order.lab} and
    \code{height} when \code{w}, the next argument is not specified.}
  \item{w}{non-negative numeric vector of bar widths.}
  \item{fromLeft}{logical, indicating if the banner is from the left or not.}
  \item{main,sub}{main and sub titles, see \code{\link{title}}.}
  \item{xlab}{x axis label (with \sQuote{correct} default e.g. for
    \code{plot.agnes}).}
  \item{adj}{passed to \code{\link{title}(main,sub)} for string adjustment.}
  \item{col}{vector of length 2, for two horizontal segments.}
  \item{border}{color for bar border; now defaults to background (no border).}
  \item{axes}{logical indicating if axes (and labels) should be drawn at all.}
  \item{frame.plot}{logical indicating the banner should be framed;
    mainly used when \code{border = 0} (as per default).}
  \item{rev.xax}{logical indicating if the x axis should be reversed (as
    in \code{plot.diana}).}
  \item{xax.pretty}{logical or integer indicating if
    \code{\link{pretty}()} should be used for the x axis.
    \code{xax.pretty = FALSE} is mainly for back compatibility.}
  \item{labels}{labels to use on y-axis; the default is constructed from
    \code{x}.}
  \item{nmax.lab}{integer indicating the number of labels which is
    considered too large for single-name labelling the banner plot.}
  \item{max.strlen}{positive integer giving the length to which
    strings are truncated in banner plot labeling.}
  \item{yax.do}{logical indicating if a y axis and banner labels should
    be drawn.}
  \item{yaxRight}{logical indicating if the y axis is on the right or left.}
  \item{y.mar}{positive number specifying the margin width to use when
    banners are labeled (along a y-axis).  The default adapts to the
    string width and optimally would also dependend on the font.}
  \item{\dots}{graphical parameters (see \code{\link{par}}) may also
    be supplied as arguments to this function.}
}
\author{Martin Maechler (from original code of Kaufman and Rousseeuw).}
\note{This is mainly a utility called from \code{\link{plot.agnes}},
  \code{\link{plot.diana}} and \code{\link{plot.mona}}.
}% also serves as \seealso{*}
\examples{
data(agriculture)
bannerplot(agnes(agriculture), main = "Bannerplot")
}
\keyword{hplot}
\keyword{cluster}
\keyword{utilities}
