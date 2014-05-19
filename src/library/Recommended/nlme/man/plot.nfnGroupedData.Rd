% File nlme/man/plot.nfnGroupedData.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: plot.nfnGroupedData.Rd,v 1.7.2.1 2003/04/17 22:28:45 bates Exp $
\name{plot.nfnGroupedData}
\title{Plot an nfnGroupedData Object}
\usage{
\method{plot}{nfnGroupedData}(x, outer, inner, innerGroups, xlab, ylab, strip, aspect, panel,
     key, grid, \dots)
}
\alias{plot.nfnGroupedData}
\arguments{
 \item{x}{an object inheriting from class \code{nfnGroupedData},
   representing a \code{groupedData} object with a numeric primary
   covariate and a single grouping level.
 }
 \item{outer}{an optional logical value or one-sided formula,
   indicating covariates that are outer to the grouping factor, which
   are used to determine the panels of the Trellis plot. If
   equal to \code{TRUE}, \code{attr(object, "outer")} is used to
   indicate the outer covariates. An outer covariate is invariant within
   the sets of rows defined by the grouping factor.  Ordering of the
   groups is done in such a way as to preserve adjacency of groups with
   the same value of the outer variables. Defaults to \code{NULL},
   meaning that no outer covariates are to be used.
 }  
 \item{inner}{an optional logical value or one-sided formula, indicating
   a covariate that is inner to the grouping factor, which is used to
   associate points within each panel of the Trellis plot. If
   equal to \code{TRUE}, \code{attr(object, "inner")} is used to
   indicate the inner covariate. An inner covariate can change within
   the sets of rows defined by the grouping  factor. Defaults to
   \code{NULL}, meaning that no inner covariate  is present. 
 } 
 \item{innerGroups}{an optional one-sided formula specifying a factor
   to be used for grouping the levels of the \code{inner}
   covariate. Different colors, or line types, are used for each level
   of the \code{innerGroups} factor. Default is \code{NULL}, meaning
   that no \code{innerGroups} covariate is present.
 }
 \item{xlab, ylab}{optional character strings with the labels for the
   plot. Default is the corresponding elements of \code{attr(object,
     "labels")} and \code{attr(object, "units")} pasted together.
 }
 \item{strip}{an optional function passed as the \code{strip} argument to
   the \code{xyplot} function. Default is \code{strip.default(\dots, style
     = 1)} (see \code{trellis.args}).
   }
 \item{aspect}{an optional character string indicating the aspect ratio
   for the plot passed as the \code{aspect} argument to the
   \code{xyplot} function. Default is \code{"xy"} (see
   \code{trellis.args}). 
 }
 \item{panel}{an optional function used to generate the individual
   panels in the Trellis display, passed as the \code{panel} argument
   to the \code{xyplot} function.}
 \item{key}{an optional logical function or function. If \code{TRUE}
   and \code{innerGroups} is non-\code{NULL}, a legend for the
   different \code{innerGroups} levels is included at the top of the
   plot. If given as a function, it is passed as the \code{key} argument
   to the \code{xyplot} function.  Default is \code{TRUE} if
   \code{innerGroups} is non-\code{NULL} and \code{FALSE} otherwise.
 }
 \item{grid}{an optional logical value indicating whether a grid should
   be added to plot. Default is \code{TRUE}.}
 \item{\dots}{optional arguments passed to the \code{xyplot} function.}
}
\description{
  A Trellis plot of the response versus the primary covariate is
  generated. If outer variables are specified, the combination of their
  levels are used to determine the panels of the Trellis
  display. Otherwise, the levels of the grouping variable determine the
  panels. A scatter plot of the response versus the primary covariate is
  displayed in each panel, with observations corresponding to same
  inner group joined by line segments. The Trellis function
  \code{xyplot} is used. 
}
\value{
  a Trellis plot of the response versus the primary covariate.
}

\references{
Bates, D.M. and Pinheiro, J.C. (1997), "Software Design for Longitudinal
Data", in "Modelling Longitudinal and Spatially Correlated Data:
Methods, Applications and Future Directions", T.G. Gregoire (ed.),
Springer-Verlag, New York.
}
\author{José Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{groupedData}}, \code{\link{xyplot}}}
\examples{
# different panels per Subject
plot(Orthodont)
# different panels per gender
plot(Orthodont, outer = TRUE)
}
\keyword{models}
