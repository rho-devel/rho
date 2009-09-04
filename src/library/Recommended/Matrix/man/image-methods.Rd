\name{image-methods}
\title{Methods for image() in Package 'Matrix'}
\docType{methods}
%\alias{image}% <- needed for \usage{.}
\alias{image-methods}
\alias{image,ANY-method}
\alias{image,CHMfactor-method}
\alias{image,Matrix-method}
\alias{image,dgRMatrix-method}
\alias{image,dgCMatrix-method}
\alias{image,dgTMatrix-method}
\alias{image,dsparseMatrix-method}
\alias{image,lsparseMatrix-method}
\alias{image,nsparseMatrix-method}
\description{
  Methods for function \code{\link[graphics]{image}} in package
  \pkg{Matrix}.  An image of a matrix simply color codes all matrix
  entries and draws the \eqn{n\times m}{n x m} matrix using an
  \eqn{n\times m}{n x m} grid of (colored) rectangles.
}
\usage{% want \usage{} since we have many "surprising arguments"
\S4method{image}{dgTMatrix}(x,
      xlim = .5 + c(0, di[2]),
      ylim = .5 + c(di[1], 0), aspect = "iso",
      sub = sprintf("Dimensions: \%d x \%d", di[1], di[2]),
      xlab = "Column", ylab = "Row",
      useAbs = NULL, colorkey = !useAbs,
      col.regions = NULL,
      lwd = NULL, \dots)
}
\arguments{
  \item{x}{a Matrix object, i.e., fulfilling
    \code{\link{is}(x, "Matrix")}.}
  \item{xlim, ylim}{x- and y-axis limits; may be used to \dQuote{zoom
      into} matrix.}
  \item{aspect}{aspect ratio specified as number (y/x) or string;
    see \code{\link[lattice]{levelplot}}.}
  \item{sub, xlab, ylab}{axis annotation with sensible defaults;
    see \code{\link{plot.default}}.}
  \item{useAbs}{logical indicating if \code{\link{abs}(x)} should be
    shown; if \code{TRUE}, the former (implicit) default, the default
    \code{col.regions} will be \code{\link{grey}} colors (and no
    \code{colorkey} drawn).  The default is \code{FALSE} unless the
    matrix has no negative entries.}
  \item{colorkey}{logical indicating if a color key aka \sQuote{legend}
    should be produced.  Default is to draw one, unless \code{useAbs} is
    true.}
  \item{col.regions}{vector of gradually varying colors; see
    \code{\link[lattice]{levelplot}}.}
  \item{lwd}{non-negative number or \code{NULL} (default), specifying the
    line-width of the rectangles of each non-zero matrix entry (drawn by
    \code{\link[grid]{grid.rect}}).  The default depends on the matrix
    dimension and the device size.}
  \item{\dots}{further arguments passed to methods and
    \code{\link[lattice]{levelplot}}.}
}
\section{Methods}{
  All methods currently end up calling the method for the
  \code{\linkS4class{dgTMatrix}} class.
  Use \code{showMethods(image)} to list them all.
}
\examples{
showMethods(image)
## If you want to see all the methods' implementations:
showMethods(image, incl=TRUE, inherit=FALSE)

data(CAex)
image(CAex, main = "image(CAex)")
image(CAex, useAbs=TRUE, main = "image(CAex, useAbs=TRUE)")

cCA <- Cholesky(crossprod(CAex), Imult = .01)
image(cCA, main="Cholesky(crossprod(CAex), Imult = .01)")
image(cCA, useAbs=TRUE)

data(USCounties)
image(USCounties)# huge
image(sign(USCounties))## just the pattern
    # how the result looks, may depend heavily on
    # the device, screen resolution, antialiasing etc
    # e.g. x11(type="Xlib") may show very differently than cairo-based

## Drawing borders around each rectangle;
    # again, viewing depends very much on the device:
image(USCounties[1:400,1:200], lwd=.1)
image(USCounties[1:200,1:200], lwd=.5)
image(USCounties[1:200,1:200], lwd=.01)
}
\keyword{methods}
\keyword{hplot}
