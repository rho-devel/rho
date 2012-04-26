% File nlme/man/plot.nffGroupedData.Rd
% Part of the nlme package for R
% Distributed under GPL 2 or later: see nlme/LICENCE

% $Id: plot.nffGroupedData.Rd,v 1.7.2.1 2003/04/17 22:28:45 bates Exp $
\name{plot.nffGroupedData}
\title{Plot an nffGroupedData Object}
\usage{
\method{plot}{nffGroupedData}(x, outer, inner, innerGroups, xlab, ylab, strip, panel, key,
     grid, \dots)
}
\alias{plot.nffGroupedData}
\arguments{
 \item{x}{an object inheriting from class \code{nffGroupedData},
   representing a \code{groupedData} object with a factor primary
   covariate and a single grouping level.
 }
 \item{outer}{an optional logical value or one-sided formula,
   indicating covariates that are outer to the grouping factor, which
   are used to determine the panels of the Trellis plot. If
   equal to \code{TRUE}, \code{attr(object, "outer")} is used to indicate the
   outer covariates. An outer covariate is invariant within the sets
   of rows defined by the grouping factor.  Ordering of the groups is
   done in such a way as to preserve adjacency of groups with the same
   value of the outer variables. Defaults to \code{NULL}, meaning that
   no outer covariates are to be used.
 }  
 \item{inner}{an optional logical value or one-sided formula, indicating
   a covariate that is inner to the grouping factor, which is used to
   associate points within each panel of the Trellis plot. If
   equal to \code{TRUE}, \code{attr(object, "inner")} is used to indicate the
   inner covariate. An inner covariate can change within the sets of
   rows defined by the grouping  factor. Defaults to \code{NULL},
   meaning that no inner covariate  is present. 
 } 
 \item{innerGroups}{an optional one-sided formula specifying a factor
   to be used for grouping the levels of the \code{inner}
   covariate. Different colors, or symbols, are used for each level
   of the \code{innerGroups} factor. Default is \code{NULL}, meaning
   that no \code{innerGroups} covariate is present.
 }
 \item{xlab}{an optional character string with the label for the
   horizontal axis. Default is the \code{y} elements of \code{attr(object,
     "labels")} and \code{attr(object, "units")} pasted together.
 }
 \item{ylab}{an optional character string with the label for the
   vertical axis. Default is the grouping factor name.
 }
 \item{strip}{an optional function passed as the \code{strip} argument to
   the \code{dotplot} function. Default is \code{strip.default(..., style
     = 1)} (see \code{trellis.args}).
 }
 \item{panel}{an optional function used to generate the individual
   panels in the Trellis display, passed as the \code{panel} argument
   to the \code{dotplot} function.
 }
 \item{key}{an optional logical function or function. If \code{TRUE}
   and either \code{inner} or \code{innerGroups} are non-\code{NULL}, a
   legend for the different \code{inner} (\code{innerGroups}) levels is 
   included at the top of the plot. If given as a function, it is passed
   as the \code{key} argument to the \code{dotplot} function. Default is
   \code{TRUE} is either \code{inner} or \code{innerGroups} are
   non-\code{NULL} and \code{FALSE} otherwise.
 }
 \item{grid}{this argument is included for consistency with the
         \code{plot.nfnGroupedData} method calling sequence. It is
         ignored in this method function.} 
 \item{\dots}{optional arguments passed to the \code{dotplot} function.}
}
\description{
  A Trellis dot-plot of the response by group is generated. If outer
  variables are specified, the combination of their levels are used to
  determine the panels of the Trellis display. The Trellis function
  \code{dotplot} is used.
}
\value{
  a Trellis dot-plot of the response by group.
}

\references{
Bates, D.M. and Pinheiro, J.C. (1997), "Software Design for Longitudinal
Data", in "Modelling Longitudinal and Spatially Correlated Data:
Methods, Applications and Future Directions", T.G. Gregoire (ed.),
Springer-Verlag, New York.
}
\author{Jose Pinheiro and Douglas Bates \email{bates@stat.wisc.edu}}
\seealso{\code{\link{groupedData}}, \code{\link{dotplot}}}
\examples{
plot(Machines)
plot(Machines, inner = TRUE)
}
\keyword{models}
