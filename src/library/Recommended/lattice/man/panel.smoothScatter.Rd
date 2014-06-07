\name{F_2_panel.smoothScatter}
\alias{panel.smoothScatter}
\title{Lattice panel function analogous to smoothScatter}
\description{
  This function allows the user to place \code{smoothScatter} plots in
  lattice graphics.
}
\usage{
panel.smoothScatter(x, y = NULL,
                    nbin = 64, cuts = 255,
                    bandwidth,
                    colramp,
                    nrpoints = 100,
                    transformation = function(x) x^0.25,
                    pch = ".",
                    cex = 1, col="black",
                    range.x,
                    ...,
                    raster = FALSE,
                    subscripts,
                    identifier = "smoothScatter")
}
\arguments{
  \item{x}{Numeric vector containing x-values or n by 2 matrix containing x
    and y values.}
  \item{y}{Numeric vector containing y-values (optional). The length of \code{x}
    must be the same as that of \code{y}.}
  \item{nbin}{Numeric vector of length 1 (for both directions) or
    2 (for x and y separately) containing the number of equally spaced
    grid points for the density estimation.}
  \item{cuts}{ number of cuts defining the color gradient }
  \item{bandwidth}{Numeric vector: the smoothing bandwidth.  If missing,
    these functions come up with a more or less useful guess.  This
    parameter then gets passed on to the function
    \code{\link[KernSmooth:bkde2D]{bkde2D}}.}
  \item{colramp}{Function accepting an integer \code{n} as an argument and
    returning \code{n} colors.}
  \item{nrpoints}{Numeric vector of length 1 giving number of points to
    be superimposed on the density image. The first \code{nrpoints} points
    from those areas of lowest regional densities will be plotted. Adding
    points to the plot allows for the identification of outliers.
    If all points are to be plotted, choose \code{nrpoints = Inf}.}
  \item{transformation}{Function that maps the density scale to the color scale.}
  \item{pch, cex}{ graphical parameters for the \code{nrpoints}
    \dQuote{outlying} points shown in the display }
  \item{range.x}{see  \code{\link[KernSmooth:bkde2D]{bkde2D}} for details.}
  \item{col}{\code{\link{points}} color parameter}
  \item{\dots}{Further arguments that are passed on to
    \code{\link{panel.levelplot}}.}
  \item{raster}{ logical; if \code{TRUE},
    \code{\link{panel.levelplot.raster}} is used, making potentially
    smaller output files. }
  \item{subscripts}{ ignored, but necessary for handling of \dots in
    certain situations.  Likely to be removed in future. }
  \item{identifier}{
    A character string that is prepended to the names of grobs
    that are created by this panel function.
  }
}

\details{
  This replicates the display part of the \code{smoothScatter}
  function by replacing standard graphics calls by grid-compatible ones.
}

\value{
  The function is called for its side effects, namely the production
  of the appropriate plots on a graphics device.
}

\author{ Deepayan Sarkar \email{deepayan.sarkar@r-project.org} }
\examples{

ddf <- as.data.frame(matrix(rnorm(40000), ncol = 4) + 3 * rnorm(10000))
ddf[, c(2,4)] <- (-ddf[, c(2,4)])
xyplot(V1 ~ V2 + V3, ddf, outer = TRUE,
       panel = panel.smoothScatter, aspect = "iso")
splom(ddf, panel = panel.smoothScatter, nbin = 64, raster = TRUE)

}
\keyword{ hplot }

