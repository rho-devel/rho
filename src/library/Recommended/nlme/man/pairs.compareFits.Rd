% File nlme/man/pairs.compareFits.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: pairs.compareFits.Rd,v 1.9 2002/04/16 14:28:53 bates Exp $
\name{pairs.compareFits}
\title{Pairs Plot of compareFits Object}
\usage{
\method{pairs}{compareFits}(x, subset, key, \dots)
}
\alias{pairs.compareFits}
\arguments{
 \item{x}{an object of class \code{compareFits}.}
 \item{subset}{an optional logical or integer vector specifying which
   rows of \code{x} should be used in the plots. If missing, all
   rows are used.}
 \item{key}{an optional logical value, or list. If \code{TRUE}, a legend
 is included at the top of the plot indicating which symbols (colors)
 correspond to which objects being compared. If \code{FALSE}, no legend
 is included. If given as a list, \code{key} is passed down as an
 argument to the \code{trellis} function generating the plots
 (\code{splom} or  \code{xyplot}). Defaults to \code{TRUE}.}
 \item{\dots}{optional arguments passed down to the \code{trellis}
   function generating the plots.}
}
\description{
  Scatter plots of the values being compared are generated for each pair
  of coefficients in \code{x}. Different symbols (colors) are used
  for each object being compared and values corresponding to the same
  group are joined by a line, to facilitate comparison of fits. If only
  two coefficients are present, the \code{trellis} function
  \code{xyplot} is used; otherwise the \code{trellis} function \code{splom}
  is used.
}
\value{
  Pairwise scatter plots of the values being compared, with different
  symbols (colors) used for each object under comparison.
}
\author{Jose Pinheiro and Douglas Bates }
\seealso{
  \code{\link{compareFits}},
  \code{\link{plot.compareFits}},
  \code{\link{pairs.lme}},
  \code{\link{pairs.lmList}},
  \code{\link{xyplot}},
  \code{\link{splom}}
}
\examples{
fm1 <- lmList(Orthodont)
fm2 <- lme(Orthodont)
pairs(compareFits(coef(fm1), coef(fm2)))
}
\keyword{models}
